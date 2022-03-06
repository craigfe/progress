(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Line_intf
module Primitives = Line_primitives
open! Import

(** [Line] is a higher-level wrapper around [Segment] that makes some
    simplifying assumptions about progress bar rendering:

    - the reported value has a monoid instance, used for initialisation and
      accumulation.

    - the line is wrapped inside a single box.

    It contains its own notion of "accumulated" line segments, and ensures that
    this works with conditional rendering of segments (e.g. when rendering with
    a minimum interval). *)

module Acc = struct
  type 'a t =
    { mutable latest : 'a
    ; mutable accumulator : 'a
    ; mutable pending : 'a
    ; render_start : Mtime.t
    ; flow_meter : 'a Flow_meter.t
    }

  let wrap :
      type a.
         elt:(module Integer.S with type t = a)
      -> clock:(unit -> Mtime.t)
      -> should_update:(unit -> bool)
      -> a t Primitives.t
      -> a Primitives.t =
   fun ~elt:(module Integer) ~clock ~should_update inner ->
    Primitives.stateful (fun () ->
        let flow_meter =
          Flow_meter.create ~clock ~size:32 ~elt:(module Integer)
        in
        let render_start = clock () in
        let state =
          { latest = Integer.zero
          ; accumulator = Integer.zero
          ; pending = Integer.zero
          ; render_start
          ; flow_meter
          }
        in
        Primitives.contramap ~f:(fun a ->
            Flow_meter.record state.flow_meter a;
            state.pending <- Integer.add a state.pending)
        @@ Primitives.conditional (fun _ -> should_update ())
        (* On finalisation, we must flush the [pending] accumulator to get the
           true final value. *)
        @@ Primitives.on_finalise ()
        @@ Primitives.contramap ~f:(fun () ->
               let to_record = state.pending in
               state.accumulator <- Integer.add to_record state.accumulator;
               state.latest <- to_record;
               state.pending <- Integer.zero;
               state)
        @@ inner)

  let accumulator t = t.accumulator
  let flow_meter t = t.flow_meter
end

module Timer = struct
  type 'a t = { mutable render_latest : Mtime.t }

  let should_update ~interval ~clock t =
    match interval with
    | None -> Staged.inj (fun () -> true)
    | Some interval ->
        Staged.inj (fun () ->
            let now = clock () in
            match
              Mtime.Span.compare (Mtime.span t.render_latest now) interval >= 0
            with
            | false -> false
            | true ->
                t.render_latest <- now;
                true)
end

type 'a t =
  | Noop
  | Primitive of 'a Primitives.t
  | Basic of 'a Primitives.t
  | Map of ('a Primitives.t -> 'a Primitives.t) * 'a t
  | List of 'a t list
  | Contramap : ('b t * ('a -> 'b)) -> 'a t
  | Pair : 'a t * unit t * 'b t -> ('a * 'b) t
  | Acc of
      { segment : 'a Acc.t Primitives.t
      ; elt : (module Integer.S with type t = 'a)
      }

module Integer_independent (Platform : Platform.S) = struct
  open struct
    module Clock = Platform.Clock
  end

  let noop () = Noop

  let const s =
    let len = String.length s and width = Terminal.guess_printed_width s in
    let segment =
      Primitives.theta ~width (fun buf _ ->
          Line_buffer.add_substring buf s ~off:0 ~len)
    in
    Basic segment

  let spacer n = const (String.make n ' ')

  (* Like [Format.str_formatter], but with [Fmt] set to use [`Ansi_tty] style rendering. *)
  let str_formatter_buf, str_formatter =
    let buf = Buffer.create 0 in
    let ppf = Format.formatter_of_buffer buf in
    Fmt.set_style_renderer ppf `Ansi_tty;
    (buf, ppf)

  let constf fmt =
    Fmt.kpf
      (fun ppf ->
        Format.pp_print_flush ppf ();
        let str = Buffer.contents str_formatter_buf in
        Buffer.clear str_formatter_buf;
        const str)
      str_formatter fmt

  let pair ?(sep = noop ()) a b = Pair (a, sep, b)

  let list ?(sep = const " ") xs =
    let xs =
      ListLabels.filter_map xs ~f:(function Noop -> None | x -> Some x)
    in
    List (List.intersperse ~sep xs)

  let ( ++ ) a b = List [ a; b ]
  let parens t = const "(" ++ t ++ const ")"
  let brackets t = const "[" ++ t ++ const "]"
  let braces t = const "{" ++ t ++ const "}"
  let using f x = Contramap (x, f)

  let string =
    let segment =
      Primitives.alpha_unsized ~initial:(`Val "") (fun ~width buf _ s ->
          let output_len = width () - 1 (* XXX: why is -1 necessary? *) in
          if output_len <= 0 then 0
          else
            let pp =
              Staged.prj
              @@ Printer.Internals.to_line_printer
                   (Printer.string ~width:output_len)
            in
            pp buf s;
            output_len)
    in
    Basic segment

  let lpad sz t = Map (Primitives.box_fixed ~pad:`left sz, t)
  let rpad sz t = Map (Primitives.box_fixed ~pad:`right sz, t)

  (* Spinners *)

  let with_color_opt color buf f =
    match color with
    | None -> f ()
    | Some s ->
        Line_buffer.add_string buf Terminal.Style.(code (fg s));
        let a = f () in
        Line_buffer.add_string buf Terminal.Style.(code none);
        a

  module Modulo_counter : sig
    type t

    val create : int -> t
    val latest : t -> int
    val tick : t -> int
  end = struct
    type t = { modulus : int; mutable latest : int }

    let create modulus = { modulus; latest = 0 }
    let latest t = t.latest

    let tick t =
      t.latest <- succ t.latest mod t.modulus;
      t.latest
  end

  let debounce interval s =
    Primitives.stateful (fun () ->
        let latest = ref (Clock.now ()) in
        let should_update () =
          let now = Clock.now () in
          match Mtime.Span.compare (Mtime.span !latest now) interval >= 0 with
          | false -> false
          | true ->
              latest := now;
              true
        in
        Primitives.conditional (fun _ -> should_update ()) s)

  module Spinner = struct
    type t = { frames : string array; final_frame : string option; width : int }

    let v ~frames ~final_frame ~width = { frames; final_frame; width }

    let default =
      v ~final_frame:(Some "✔️") ~width:1
        ~frames:[| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

    let stage_count t = Array.length t.frames
  end

  let spinner ?frames ?color ?(min_interval = Some (Duration.of_int_ms 80)) () =
    let spinner =
      match frames with
      | None -> Spinner.default
      | Some [] -> Fmt.invalid_arg "spinner must have at least one stage"
      | Some (x :: xs as frames) ->
          let width = Terminal.guess_printed_width x in
          ListLabels.iteri xs ~f:(fun i x ->
              let width' = Terminal.guess_printed_width x in
              if width <> width' then
                Fmt.invalid_arg
                  "Spinner frames must have the same UTF-8 length. found %d \
                   (at index 0) and %d (at index %d)"
                  (i + 1) width width');
          Spinner.v ~frames:(Array.of_list frames) ~final_frame:None ~width
    in
    let apply_debounce = Option.fold min_interval ~none:Fun.id ~some:debounce in
    Basic
      (Primitives.stateful (fun () ->
           let counter = Modulo_counter.create (Spinner.stage_count spinner) in
           apply_debounce
           @@ Primitives.theta ~width:spinner.Spinner.width (fun buf -> function
                | `finish ->
                    let final_frame =
                      match spinner.final_frame with
                      | None -> spinner.frames.(Modulo_counter.tick counter)
                      | Some x -> x
                    in
                    with_color_opt color buf (fun () ->
                        Line_buffer.add_string buf final_frame)
                | (`report | `tick | `rerender) as e ->
                    let tick =
                      match e with
                      | `report | `tick -> Modulo_counter.tick counter
                      | `rerender -> Modulo_counter.latest counter
                    in
                    let frame = spinner.Spinner.frames.(tick) in
                    with_color_opt color buf (fun () ->
                        Line_buffer.add_string buf frame))))
end

module Bar_style = struct
  type t =
    { delimiters : (string * string) option
    ; blank_space : string
    ; full_space : string
    ; in_progress_stages : string array
    ; color : Terminal.Color.t option
    ; color_empty : Terminal.Color.t option
    ; total_delimiter_width : int
    ; segment_width : int
    }

  let ascii =
    { delimiters = Some ("[", "]")
    ; blank_space = "-"
    ; full_space = "#"
    ; in_progress_stages = [||]
    ; color = None
    ; color_empty = None
    ; total_delimiter_width = 2
    ; segment_width = 1
    }

  let utf8 =
    { delimiters = Some ("│", "│")
    ; blank_space = " "
    ; full_space = "█"
    ; in_progress_stages = [| " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉" |]
    ; color = None
    ; color_empty = None
    ; total_delimiter_width = 2
    ; segment_width = 1
    }

  let parse_stages ctx = function
    | [] -> Fmt.invalid_arg "%s: empty list of bar stages supplied" ctx
    | full_space :: xs ->
        let segment_width = Terminal.guess_printed_width full_space in
        if segment_width = 0 then
          Fmt.invalid_arg
            "%s: supplied stage '%s' has estimated printed width of 0" ctx
            full_space;
        let in_progress_stages, blank_space =
          match List.rev xs with
          | [] -> ([||], String.make segment_width ' ')
          | blank_space :: xs -> (Array.of_list xs, blank_space)
        in
        (full_space, in_progress_stages, blank_space, segment_width)

  let guess_delims_width = function
    | None -> 0
    | Some (l, r) -> Terminal.(guess_printed_width l + guess_printed_width r)

  let v ?delims ?color ?color_empty stages =
    let full_space, in_progress_stages, blank_space, segment_width =
      parse_stages "Bar_styles.v" stages
    in
    { delimiters = delims
    ; blank_space
    ; full_space
    ; in_progress_stages
    ; color
    ; color_empty
    ; segment_width
    ; total_delimiter_width = guess_delims_width delims
    }

  let with_color color t = { t with color = Some color }
  let with_empty_color color_empty t = { t with color_empty = Some color_empty }

  let with_delims delimiters t =
    { t with delimiters; total_delimiter_width = guess_delims_width delimiters }

  let with_stages stages t =
    let full_space, in_progress_stages, blank_space, segment_width =
      parse_stages "Bar_styles.with_stages" stages
    in
    { t with full_space; blank_space; in_progress_stages; segment_width }
end

module Make (Platform : Platform.S) = struct
  open struct
    module Clock = Platform.Clock
  end

  module Integer_independent = Integer_independent (Platform)
  include Integer_independent

  module Internals = struct
    module Line_buffer = Line_buffer
    include Primitives

    let box_winsize ?max ?(fallback = 80) s =
      let get_width () =
        let real_width =
          Option.value ~default:fallback (Platform.Terminal_width.get ())
        in
        match max with None -> real_width | Some m -> min m real_width
      in
      box_dynamic get_width s

    let to_line t = Primitive t
  end

  let to_primitive : type a. Config.t -> a t -> a Primitives.t =
    let rec inner : type a. a t -> (unit -> bool) -> a Primitives.t = function
      | Noop -> fun _ -> Primitives.noop ()
      | Primitive x -> fun _ -> x
      | Pair (a, sep, b) ->
          let a = inner a in
          let sep = inner sep in
          let b = inner b in
          fun should_update ->
            Primitives.pair ~sep:(sep should_update) (a should_update)
              (b should_update)
      | Contramap (x, f) ->
          let x = inner x in
          fun y -> Primitives.contramap ~f (x y)
      | Map (f, x) -> fun a -> f (inner x a)
      | List xs ->
          let xs = List.map xs ~f:inner in
          fun should_update ->
            Primitives.array
              (List.map xs ~f:(fun f -> f should_update) |> Array.of_list)
      | Basic segment ->
          fun should_update ->
            Primitives.conditional (fun _ -> should_update ()) @@ segment
      | Acc { segment; elt = (module Integer) } ->
          fun should_update ->
            Acc.wrap ~elt:(module Integer) ~clock:Clock.now ~should_update
            @@ segment
    in
    fun (config : Config.t) -> function
      | Primitive x -> x
      | t ->
          let inner = inner t in
          let segment =
            Primitives.stateful (fun () ->
                let should_update =
                  let state = { Timer.render_latest = Clock.now () } in
                  Staged.prj
                    (Timer.should_update ~clock:Clock.now
                       ~interval:config.min_interval state)
                in
                let x = ref true in
                Primitives.contramap ~f:(fun a ->
                    x := should_update ();
                    a)
                @@ Internals.box_winsize ?max:config.max_width
                @@ inner (fun () -> !x))
          in
          segment

  (* Basic utilities for combining segments *)

  module Integer_dependent = struct
    module type S =
      Integer_dependent
        with type 'a t := 'a t
         and type color := Terminal.Color.t
         and type duration := Duration.t
         and type 'a printer := 'a Printer.t
         and type bar_style := Bar_style.t

    module type Ext =
      DSL
        with type 'a t := 'a t
         and type color := Terminal.Color.t
         and type duration := Duration.t
         and type 'a printer := 'a Printer.t
         and type Bar_style.t := Bar_style.t

    module Make_ext (Integer : Integer.S) = struct
      let acc segment = Acc { segment; elt = (module Integer) }

      let of_printer ?init printer =
        let pp = Staged.prj @@ Printer.Internals.to_line_printer printer in
        let width = Printer.print_width printer in
        let initial =
          match init with
          | Some v -> `Val v
          | None ->
              `Theta
                (fun buf ->
                  for _ = 1 to width do
                    Line_buffer.add_char buf ' '
                  done)
        in
        Basic (Primitives.alpha ~width ~initial (fun buf _ x -> pp buf x))

      let count_pp printer =
        let pp = Staged.prj @@ Printer.Internals.to_line_printer printer in
        acc
        @@ Primitives.contramap ~f:Acc.accumulator
        @@ Primitives.alpha ~width:(Printer.print_width printer)
             ~initial:(`Val Integer.zero) (fun buf _ x -> pp buf x)

      let bytes = count_pp (Units.Bytes.generic (module Integer))

      let percentage_of accumulator =
        let printer =
          Printer.using Units.Percentage.of_float ~f:(fun x ->
              Integer.to_float x /. Integer.to_float accumulator)
        in
        count_pp printer

      let sum ?pp ~width () =
        let pp =
          match pp with
          | None -> Printer.Internals.integer ~width (module Integer)
          | Some x -> x
        in
        let pp = Staged.prj (Printer.Internals.to_line_printer pp) in
        acc
        @@ Primitives.contramap ~f:Acc.accumulator
        @@ Primitives.alpha ~initial:(`Val Integer.zero) ~width (fun buf _ x ->
               pp buf x)

      let count_to ?pp ?(sep = const "/") total =
        let total = Integer.to_string total in
        let width =
          match pp with
          | Some pp -> Printer.print_width pp
          | None -> String.length total
        in
        List [ sum ~width (); using (fun _ -> ()) sep; const total ]

      let ticker_to ?(sep = const "/") total =
        let total = Integer.to_string total in
        let width = String.length total in
        let pp =
          Staged.prj
          @@ Printer.Internals.to_line_printer
          @@ Printer.Internals.integer ~width (module Integer)
        in
        let segment =
          Primitives.alpha ~width ~initial:(`Val Integer.zero) (fun buf _ x ->
              pp buf x)
        in
        List
          [ Contramap
              ( Acc
                  { segment = Primitives.contramap ~f:Acc.accumulator segment
                  ; elt = (module Integer)
                  }
              , fun _ -> Integer.one )
          ; using (fun _ -> ()) sep
          ; const total
          ]

      (* Progress bars *)

      module Bar_style = Bar_style

      let bar (spec : Bar_style.t) width proportion buf =
        let final_stage = Array.length spec.in_progress_stages in
        let width = width () in
        let bar_segments =
          (width - spec.total_delimiter_width) / spec.segment_width
        in
        let squaresf = Float.of_int bar_segments *. proportion in
        let squares = Float.to_int squaresf in
        let filled = min squares bar_segments in
        let not_filled =
          bar_segments - filled - if final_stage = 0 then 0 else 1
        in
        Option.iter (fun (x, _) -> Line_buffer.add_string buf x) spec.delimiters;
        with_color_opt spec.color buf (fun () ->
            for _ = 1 to filled do
              Line_buffer.add_string buf spec.full_space
            done);
        let () =
          if filled <> bar_segments then (
            let chunks = Float.to_int (squaresf *. Float.of_int final_stage) in
            let index = chunks - (filled * final_stage) in
            if index >= 0 && index < final_stage then
              with_color_opt spec.color buf (fun () ->
                  Line_buffer.add_string buf spec.in_progress_stages.(index));

            with_color_opt spec.color_empty buf (fun () ->
                for _ = 1 to not_filled do
                  Line_buffer.add_string buf spec.blank_space
                done))
        in
        Option.iter (fun (_, x) -> Line_buffer.add_string buf x) spec.delimiters;
        width

      let with_prop f v t = match v with None -> t | Some v -> f v t

      let bar ~style ~color =
        let style =
          match style with
          | `ASCII -> Bar_style.ascii
          | `UTF8 -> Bar_style.utf8
          | `Custom style -> style
        in
        bar (style |> with_prop Bar_style.with_color color)

      let bar ?(style = `ASCII) ?color ?(width = `Expand) ?(data = `Sum) total =
        let proportion x = Integer.to_float x /. Integer.to_float total in
        let proportion_segment =
          match width with
          | `Fixed width ->
              if width < 3 then failwith "Not enough space for a progress bar";
              Primitives.alpha ~width ~initial:(`Val 0.) (fun buf _ x ->
                  ignore (bar ~style ~color (fun _ -> width) x buf : int))
          | `Expand ->
              Primitives.alpha_unsized ~initial:(`Val 0.) (fun ~width ppf _ x ->
                  bar ~style ~color width x ppf)
        in
        match data with
        | `Latest ->
            Basic (Primitives.contramap proportion_segment ~f:proportion)
        | `Sum ->
            acc
              (Primitives.contramap proportion_segment
                 ~f:(Acc.accumulator >> proportion))

      let rate pp_val =
        let pp_rate =
          let pp_val = Staged.prj (Printer.Internals.to_line_printer pp_val) in
          fun buf _ x ->
            pp_val buf x;
            Line_buffer.add_string buf "/s"
        in
        let width = Printer.print_width pp_val + 2 in
        acc
        @@ Primitives.contramap ~f:(Acc.flow_meter >> Flow_meter.per_second)
        @@ Primitives.alpha ~width ~initial:(`Val 0.) pp_rate

      let bytes_per_sec = rate Units.Bytes.of_float

      let eta ?(pp = Units.Duration.mm_ss) total =
        let span_segment =
          let printer =
            let pp = Staged.prj (Printer.Internals.to_line_printer pp) in
            fun ppf event x ->
              match event with
              | `finish -> pp ppf Mtime.Span.max_span (* renders as [--:--] *)
              | `report | `rerender
              | `tick
                (* TODO: tick should cause the estimate to be re-evaluated. *)
                ->
                  pp ppf x
          in
          let width = Printer.print_width Units.Duration.mm_ss in
          let initial = `Val Mtime.Span.max_span in
          Primitives.alpha ~width ~initial printer
        in
        acc
        @@ Primitives.contramap ~f:(fun acc ->
               let per_second = Flow_meter.per_second (Acc.flow_meter acc) in
               let acc = Acc.accumulator acc in
               if Float.equal 0. per_second then Mtime.Span.max_span
               else
                 let todo = Integer.(to_float (sub total acc)) in
                 if Float.(todo <= 0.) then Mtime.Span.zero
                 else
                   Mtime.Span.of_uint64_ns
                     (Int64.of_float (todo /. per_second *. 1_000_000_000.)))
        @@ span_segment

      let elapsed ?(pp = Units.Duration.mm_ss) () =
        let print_time = Staged.prj (Printer.Internals.to_line_printer pp) in
        let width = Printer.print_width pp in
        let segment =
          Primitives.stateful (fun () ->
              let elapsed = Clock.counter () in
              let latest = ref Mtime.Span.zero in
              let finished = ref false in
              let pp buf e =
                (match e with
                | `tick | `report -> latest := Clock.count elapsed
                | `finish when not !finished ->
                    latest := Clock.count elapsed;
                    finished := true
                | `rerender | `finish -> ());
                print_time buf !latest
              in
              Primitives.theta ~width pp)
        in
        Basic segment

      include Integer_independent
    end

    module Make = Make_ext
  end

  include Integer_dependent.Make (Integer.Int)
  module Using_int32 = Integer_dependent.Make_ext (Integer.Int32)
  module Using_int63 = Integer_dependent.Make_ext (Integer.Int63)
  module Using_int64 = Integer_dependent.Make_ext (Integer.Int64)
  module Using_float = Integer_dependent.Make_ext (Integer.Float)
end

(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
  ————————————————————————————————————————————————————————————————————————————*)
