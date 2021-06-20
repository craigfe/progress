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

let update_only pp ppf = function
  | `start -> ()
  | `finish x | `report x | `rerender x -> pp ppf x

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
    let len = String.length s and len_utf8 = String.Utf8.length s in
    let segment =
      Primitives.theta ~width:len_utf8 (fun buf _ ->
          Line_buffer.add_substring buf s ~off:0 ~len)
    in
    Basic segment

  let constf fmt = Format.kasprintf const fmt
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
      Primitives.alpha_unsized ~initial:(`Val "") (fun ~width buf -> function
        | `start -> 0 (* TODO: chosen randomly *)
        | `finish s | `report s | `rerender s ->
            let input_len = String.length s in
            let output_len = width () - 1 (* XXX: why is -1 necessary? *) in
            let () =
              match input_len <= output_len with
              | true ->
                  Line_buffer.add_string buf s;
                  for _ = input_len to output_len do
                    Line_buffer.add_char buf ' '
                  done
              | false ->
                  Line_buffer.add_substring buf s ~off:0 ~len:(output_len - 4);
                  Line_buffer.add_string buf " ..."
            in
            output_len)
    in
    Basic segment

  let lpad sz t = Map (Primitives.box_fixed ~pad:`left sz, t)
  let rpad sz t = Map (Primitives.box_fixed ~pad:`right sz, t)

  let ticker () =
    let segment =
      let pp ~width buf count =
        let count =
          match count with
          | `finish x | `report x | `rerender x -> x
          | `start -> Int63.zero
        in
        let width = width () in
        let str = Int63.to_string count in
        let len = String.length str in
        for _ = len + 1 to width do
          (* TODO: deal with overflow *)
          Line_buffer.add_char buf ' '
        done;
        Line_buffer.add_string buf str;
        width
      in
      Primitives.alpha_unsized ~initial:(`Val Int63.zero) pp
    in
    Contramap
      ( Acc
          { segment = Primitives.contramap ~f:Acc.accumulator segment
          ; elt = (module Integer.Int63)
          }
      , fun _ -> Int63.one )

  (* Spinners *)

  let with_color_opt color buf f =
    match color with
    | None -> f ()
    | Some s ->
        Line_buffer.add_string buf Ansi.Style.(code (fg s));
        let a = f () in
        Line_buffer.add_string buf Ansi.Style.(code none);
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
        ~frames:
          [| "⠋"
           ; "⠙"
           ; "⠹"
           ; "⠸"
           ; "⠼"
           ; "⠴"
           ; "⠦"
           ; "⠧"
           ; "⠇"
           ; "⠏"
          |]

    let stage_count t = Array.length t.frames
  end

  let spinner ?color ?frames ?(min_interval = Some (Duration.of_int_ms 80)) () =
    let spinner =
      match frames with
      | None -> Spinner.default
      | Some [] -> Fmt.invalid_arg "spinner must have at least one stage"
      | Some (x :: xs as frames) ->
          let width = String.Utf8.length x in
          ListLabels.iteri xs ~f:(fun i x ->
              let width' = String.Utf8.length x in
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
                | `start -> ()
                | `finish () ->
                    let final_frame =
                      match spinner.final_frame with
                      | None -> spinner.frames.(Modulo_counter.tick counter)
                      | Some x -> x
                    in
                    with_color_opt color buf (fun () ->
                        Line_buffer.add_string buf final_frame)
                | (`report () | `rerender ()) as e ->
                    let tick =
                      match e with
                      | `report () -> Modulo_counter.tick counter
                      | `rerender () -> Modulo_counter.latest counter
                    in
                    let frame = spinner.Spinner.frames.(tick) in
                    with_color_opt color buf (fun () ->
                        Line_buffer.add_string buf frame))))
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
         and type color := Ansi.Color.t
         and type 'a printer := 'a Printer.t

    module type Ext =
      DSL
        with type 'a t := 'a t
         and type color := Ansi.Color.t
         and type 'a printer := 'a Printer.t

    module Make_ext (Integer : Integer.S) = struct
      let acc segment = Acc { segment; elt = (module Integer) }

      let of_printer ?init printer =
        let pp = update_only @@ Staged.prj (Printer.to_line_printer printer) in
        let width = Printer.width printer in
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
        Basic (Primitives.alpha ~width ~initial pp)

      let count_pp printer =
        let pp = Staged.prj (Printer.to_line_printer printer) in
        acc
        @@ Primitives.contramap ~f:Acc.accumulator
        @@ Primitives.alpha ~width:(Printer.width printer)
             ~initial:(`Val Integer.zero) (update_only pp)

      let bytes = count_pp (Units.Bytes.generic (module Integer))

      let percentage_of accumulator =
        let printer =
          Printer.using Units.Percentage.of_float ~f:(fun x ->
              Integer.to_float x /. Integer.to_float accumulator)
        in
        count_pp printer

      let count ?pp ~width () =
        let pp =
          match pp with
          | None -> Printer.integer ~width (module Integer)
          | Some x -> x
        in
        let pp = Staged.prj (Printer.to_line_printer pp) in
        acc
        @@ Primitives.contramap ~f:Acc.accumulator
        @@ Primitives.alpha ~initial:(`Val Integer.zero) ~width (update_only pp)

      let count_up_to ?pp ?(sep = const "/") total =
        let total = Integer.to_string total in
        let width =
          match pp with
          | Some pp -> Printer.width pp
          | None -> String.length total
        in
        List [ count ~width (); using (fun _ -> ()) sep; const total ]

      (* Progress bars *)

      let bar_custom ~stages ~color ~color_empty width proportion buf =
        let color_empty = Option.(color_empty || color) in
        let stages = Array.of_list stages in
        let final_stage = Array.length stages - 1 in
        let width = width () in
        let bar_width = width - 2 in
        let squaresf = Float.of_int bar_width *. proportion in
        let squares = Float.to_int squaresf in
        let filled = min squares bar_width in
        let not_filled = bar_width - filled - 1 in
        Line_buffer.add_string buf "│";
        with_color_opt color buf (fun () ->
            for _ = 1 to filled do
              Line_buffer.add_string buf stages.(final_stage)
            done);
        let () =
          if filled <> bar_width then (
            let chunks = Float.to_int (squaresf *. Float.of_int final_stage) in
            let index = chunks - (filled * final_stage) in
            if index >= 0 && index < final_stage then
              with_color_opt color buf (fun () ->
                  Line_buffer.add_string buf stages.(index));

            with_color_opt color_empty buf (fun () ->
                for _ = 1 to not_filled do
                  Line_buffer.add_string buf stages.(0)
                done))
        in
        Line_buffer.add_string buf "│";
        width

      let bar_ascii ~color ~color_empty width proportion buf =
        let color_empty = Option.(color_empty || color) in
        let width = width () in
        let bar_width = width - 2 in
        let filled =
          min (Float.to_int (Float.of_int bar_width *. proportion)) bar_width
        in
        let not_filled = bar_width - filled in
        Line_buffer.add_char buf '[';
        with_color_opt color buf (fun () ->
            for _ = 1 to filled do
              Line_buffer.add_char buf '#'
            done);
        with_color_opt color_empty buf (fun () ->
            for _ = 1 to not_filled do
              Line_buffer.add_char buf '-'
            done);
        Line_buffer.add_char buf ']';
        width

      let bar ~style =
        match style with
        | `ASCII -> bar_ascii
        | `Custom stages -> bar_custom ~stages
        | `UTF8 ->
            let stages =
              [ " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" ]
            in
            bar_custom ~stages

      (* TODO: fix duplication with below *)
      let bar_unaccumulated ?(style = `UTF8) ?color ?color_empty
          ?(width = `Expand) ~total () =
        let proportion x = Integer.to_float x /. Integer.to_float total in
        Basic
          (Primitives.contramap ~f:proportion
          @@
          match width with
          | `Fixed width ->
              if width < 3 then failwith "Not enough space for a progress bar";
              Primitives.alpha ~width ~initial:(`Val 0.) (fun buf -> function
                | `start -> ()
                | `finish x | `report x | `rerender x ->
                    ignore
                      (bar ~style ~color ~color_empty (fun _ -> width) x buf
                        : int))
          | `Expand ->
              Primitives.alpha_unsized ~initial:(`Val 0.) (fun ~width ppf ->
                function
                | `start -> 0
                | `finish x | `report x | `rerender x ->
                    bar ~style ~color ~color_empty width x ppf))

      let bar ?(style = `UTF8) ?color ?color_empty ?(width = `Expand) ~total ()
          =
        let proportion x = Integer.to_float x /. Integer.to_float total in
        let proportion_segment =
          match width with
          | `Fixed width ->
              if width < 3 then failwith "Not enough space for a progress bar";
              Primitives.alpha ~width ~initial:(`Val 0.) (fun buf -> function
                | `start -> ()
                | `finish x | `report x | `rerender x ->
                    ignore
                      (bar ~style ~color ~color_empty (fun _ -> width) x buf
                        : int))
          | `Expand ->
              Primitives.alpha_unsized ~initial:(`Val 0.) (fun ~width ppf ->
                function
                | `start -> 0
                | `finish x | `report x | `rerender x ->
                    bar ~style ~color ~color_empty width x ppf)
        in
        acc
          (Primitives.contramap proportion_segment
             ~f:(Acc.accumulator >> proportion))

      let rate pp_val =
        let pp_rate =
          let pp_val = Staged.prj (Printer.to_line_printer pp_val) in
          fun buf x ->
            pp_val buf x;
            Line_buffer.add_string buf "/s"
        in
        let width = Printer.width pp_val + 2 in
        acc
        @@ Primitives.contramap
             ~f:(Acc.flow_meter >> Flow_meter.per_second >> Integer.to_float)
        @@ Primitives.alpha ~width ~initial:(`Val 0.) (update_only pp_rate)

      let bytes_per_sec = rate Units.Bytes.of_float

      let eta ~total =
        let span_segment =
          let printer =
            let pp =
              Staged.prj (Printer.to_line_printer Units.Duration.mm_ss)
            in
            fun ppf -> function
              | `start -> ()
              | `finish _ -> pp ppf Mtime.Span.max_span (* renders as [--:--] *)
              | `report x | `rerender x -> pp ppf x
          in
          let width = Printer.width Units.Duration.mm_ss in
          let initial = `Val Mtime.Span.max_span in
          Primitives.alpha ~width ~initial printer
        in
        acc
        @@ Primitives.contramap ~f:(fun acc ->
               let per_second = Flow_meter.per_second (Acc.flow_meter acc) in
               let acc = Acc.accumulator acc in
               if Integer.(equal zero) per_second then Mtime.Span.max_span
               else
                 let todo = Integer.(to_float (sub total acc)) in
                 if Float.(todo <= 0.) then Mtime.Span.zero
                 else
                   Mtime.Span.of_uint64_ns
                     (Int64.of_float
                        (todo /. Integer.to_float per_second *. 1_000_000_000.)))
        @@ span_segment

      let elapsed () =
        let print_time =
          Staged.prj (Printer.to_line_printer Units.Duration.mm_ss)
        in
        let segment =
          Primitives.stateful (fun () ->
              let elapsed = Clock.counter () in
              let latest = ref Mtime.Span.zero in
              let finished = ref false in
              let pp buf e =
                (match e with
                | `start | `report _ -> latest := Clock.count elapsed
                | `finish _ when not !finished ->
                    latest := Clock.count elapsed;
                    finished := true
                | `rerender _ | `finish _ -> ());
                print_time buf !latest
              in
              Primitives.theta ~width:5 pp)
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
