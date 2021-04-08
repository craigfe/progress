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

    - the line is wrapped inside a single box. *)

module Acc = struct
  type 'a t =
    { mutable latest : 'a
    ; mutable accumulator : 'a
    ; mutable pending : 'a
    ; render_start : Mtime.t
    ; ring_buffer : 'a Ring_buffer.t
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
        let ring_buffer =
          Ring_buffer.create ~clock ~size:32 ~elt:(module Integer)
        in
        let render_start = clock () in
        let state =
          { latest = Integer.zero
          ; accumulator = Integer.zero
          ; pending = Integer.zero
          ; render_start
          ; ring_buffer
          }
        in

        Primitives.contramap ~f:(fun a ->
            Ring_buffer.record state.ring_buffer a;
            state.pending <- Integer.add a state.pending)
        @@ Primitives.conditional (fun _ -> should_update ())
        @@ Primitives.contramap ~f:(fun () ->
               let to_record = state.pending in
               state.accumulator <- Integer.add to_record state.accumulator;
               state.latest <- to_record;
               state.pending <- Integer.zero;
               state)
        @@ inner)

  let accumulator t = t.accumulator
  let ring_buffer t = t.ring_buffer
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

module Platform_dependent (Platform : Platform.S) = struct
  module Clock = Platform.Clock

  module Primitives = struct
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

  let compile : type a. a t -> Config.t -> a Primitives.t =
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
          let xs = List.map inner xs in
          fun should_update ->
            Primitives.array
              (List.map (fun f -> f should_update) xs |> Array.of_list)
      | Basic segment ->
          fun should_update ->
            Primitives.conditional (fun _ -> should_update ()) @@ segment
      | Acc { segment; elt = (module Integer) } ->
          fun should_update ->
            Acc.wrap ~elt:(module Integer) ~clock:Clock.now ~should_update
            @@ segment
    in
    fun t (config : Config.t) ->
      match t with
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
                @@ Primitives.box_winsize ?max:config.max_width
                @@ inner (fun () -> !x))
          in
          segment

  (* Basic utilities for combining segments *)

  let noop () = Noop

  let const s =
    let len = String.length s and len_utf8 = String.Utf8.length s in
    let segment =
      Primitives.theta ~width:len_utf8 (fun buf ->
          Line_buffer.add_substring buf s ~off:0 ~len)
    in
    Basic segment

  let constf fmt = Format.kasprintf const fmt
  let pair ?(sep = noop ()) a b = Pair (a, sep, b)

  let list ?(sep = const "  ") xs =
    let xs =
      ListLabels.filter_map xs ~f:(function Noop -> None | x -> Some x)
    in
    List (List.intersperse ~sep xs)

  let ( ++ ) a b = List [ a; b ]
  let using f x = Contramap (x, f)

  let string =
    let segment =
      Primitives.alpha_unsized ~initial:(`Val "") (fun ~width buf s ->
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

  (* Spinners *)

  let with_color_opt color buf f =
    match color with
    | None -> f ()
    | Some s ->
        Line_buffer.add_string buf Ansi.Style.(code (fg s));
        let a = f () in
        Line_buffer.add_string buf Ansi.Style.(code none);
        a

  let modulo_counter : int -> (unit -> int) Staged.t =
   fun bound ->
    let idx = ref (-1) in
    Staged.inj (fun () ->
        idx := succ !idx mod bound;
        !idx)

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

  let spinner ?color ?frames ?(min_interval = Some (Duration.of_int_ms 80)) () =
    let frames, width =
      match frames with
      | None ->
          ( [| "⠋"
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
          , 1 )
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
          (Array.of_list frames, width)
    in
    let stage_count = Array.length frames in
    let apply_debounce =
      Option.fold min_interval ~none:(fun x -> x) ~some:debounce
    in
    Basic
      (Primitives.stateful (fun () ->
           let tick = Staged.prj (modulo_counter stage_count) in
           apply_debounce
           @@ Primitives.theta ~width (fun buf ->
                  with_color_opt color buf (fun () ->
                      Line_buffer.add_string buf frames.(tick ())))))

  module Integer_dependent (Integer : Integer.S) = struct
    let acc segment = Acc { segment; elt = (module Integer) }

    let basic ~init printer =
      let pp = Staged.prj (Printer.to_line_printer printer) in
      Basic
        (Primitives.alpha ~width:(Printer.width printer) ~initial:(`Val init) pp)

    let of_printer printer =
      let pp = Staged.prj (Printer.to_line_printer printer) in
      acc
      @@ Primitives.contramap ~f:Acc.accumulator
      @@ Primitives.alpha ~width:(Printer.width printer)
           ~initial:(`Val Integer.zero) pp

    let bytes = of_printer (Units.Bytes.generic (module Integer))

    let percentage_of accumulator =
      let printer =
        Printer.using Units.Percentage.of_float ~f:(fun x ->
            Integer.to_float x /. Integer.to_float accumulator)
      in
      of_printer printer

    let max total = const (Integer.to_string total)

    let count total =
      let total = Integer.to_string total in
      let width = String.length total in
      acc
      @@ Primitives.contramap ~f:Acc.accumulator
      @@ Primitives.alpha ~initial:(`Val Integer.zero) ~width (fun lb x ->
             let x = Integer.to_string x in
             for _ = String.length x to width - 1 do
               Line_buffer.add_char lb ' '
             done;
             Line_buffer.add_string lb x)

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

    let bar_unaccumulated ?(style = `UTF8) ?color ?color_empty
        ?(width = `Expand) ~total () =
      let proportion x = Integer.to_float x /. Integer.to_float total in
      Basic
        (Primitives.contramap ~f:proportion
        @@
        match width with
        | `Fixed width ->
            if width < 3 then failwith "Not enough space for a progress bar";
            Primitives.alpha ~width ~initial:(`Val 0.) (fun buf x ->
                ignore
                  (bar ~style ~color ~color_empty (fun _ -> width) x buf : int))
        | `Expand ->
            Primitives.alpha_unsized ~initial:(`Val 0.) (fun ~width ppf x ->
                bar ~style ~color ~color_empty width x ppf))

    let bar ?(style = `UTF8) ?color ?color_empty ?(width = `Expand) ~total () =
      let proportion x = Integer.to_float x /. Integer.to_float total in
      acc
      @@ Primitives.contramap ~f:(Acc.accumulator >> proportion)
      @@
      match width with
      | `Fixed width ->
          if width < 3 then failwith "Not enough space for a progress bar";
          Primitives.alpha ~width ~initial:(`Val 0.) (fun buf x ->
              ignore
                (bar ~style ~color ~color_empty (fun _ -> width) x buf : int))
      | `Expand ->
          Primitives.alpha_unsized ~initial:(`Val 0.) (fun ~width ppf x ->
              bar ~style ~color ~color_empty width x ppf)

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
           ~f:
             (Acc.ring_buffer >> Ring_buffer.rate_per_second >> Integer.to_float)
      @@ Primitives.alpha ~width ~initial:(`Val 0.) pp_rate

    let eta ~total =
      let printer = Staged.prj (Printer.to_line_printer Units.Duration.mm_ss) in
      let width = Printer.width Units.Duration.mm_ss + 4 in
      let initial = `Val Mtime.Span.max_span in
      acc
      @@ Primitives.contramap ~f:(fun acc ->
             let per_second =
               Acc.ring_buffer acc |> Ring_buffer.rate_per_second
             in
             let acc = Acc.accumulator acc in
             if Integer.(equal zero) per_second then Mtime.Span.max_span
             else
               let todo = Integer.(to_float (sub total acc)) in
               if todo <= 0. then Mtime.Span.zero
               else
                 Mtime.Span.of_uint64_ns
                   (Int64.of_float
                      (todo /. Integer.to_float per_second *. 1_000_000_000.)))
      @@ Primitives.alpha ~width ~initial printer
  end

  include Integer_dependent (Integer.Int)

  module type Integer_dependent = sig
    include
      Integer_dependent
        with type 'a t := 'a t
         and type color := Ansi.Color.t
         and type 'a printer := 'a Printer.t
  end

  module Int32 = Integer_dependent (Integer.Int32)
  module Int64 = Integer_dependent (Integer.Int64)
  module Float = Integer_dependent (Integer.Float)

  (* TODO: common start time by using something like Acc *)
  (* let elapsed () =
   *   let print_time =
   *     Staged.prj (Printer.to_line_buffer Units.Duration.mm_ss_print)
   *   in
   *   let segment =
   *     Primitives.contramap ~f: (fun acc ->
   *         let time = Mtime.span (Acc.render_start acc) (Clock.now ()) in
   *         Primitives.theta ~width:5 (fun buf -> print_time buf time))
   *   in
   *   { segment; } *)

  let elapsed () =
    let print_time =
      Staged.prj (Printer.to_line_printer Units.Duration.mm_ss)
    in
    let segment =
      Primitives.stateful (fun () ->
          let start_time = Clock.counter () in
          let pp buf = print_time buf (Clock.count start_time) in
          Primitives.theta ~width:5 pp)
    in
    Basic segment
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
