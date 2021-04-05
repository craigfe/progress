(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Line_intf
include Line_intf.Types
module Expert = Segment
open! Import

(** [Line] is a higher-level wrapper around [Segment] that makes some
    simplifying assumptions about progress bar rendering:

    - the reported value has a monoid instance, used for initialisation and
      accumulation.

    - the line is wrapped inside a single box. *)

module Acc = struct
  type 'a t =
    { mutable latest : 'a
    ; mutable total : 'a (* TODO: rename to accumulator *)
    ; mutable pending : 'a
    ; render_start : Mtime.t
    ; ring_buffer : 'a Ring_buffer.t
    }

  let wrap :
      type a.
         elt:(module Integer.S with type t = a)
      -> clock:(unit -> Mtime.t)
      -> should_update:(unit -> bool)
      -> a t Expert.t
      -> a Expert.t =
   fun ~elt:(module Integer) ~clock ~should_update inner ->
    Expert.stateful (fun () ->
        let ring_buffer =
          Ring_buffer.create ~clock ~size:16 ~elt:(module Integer)
        in
        let render_start = clock () in
        let state =
          { latest = Integer.zero
          ; total = Integer.zero
          ; pending = Integer.zero
          ; render_start
          ; ring_buffer
          }
        in

        Expert.contramap ~f:(fun a ->
            state.pending <- Integer.add a state.pending)
        @@ Expert.conditional (fun _ -> should_update ())
        @@ Expert.contramap ~f:(fun () ->
               state.total <- Integer.add state.pending state.total;
               state.latest <- state.pending;
               state.pending <- Integer.zero;
               state)
        @@ inner)

  let total t = t.total
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
  | Basic of 'a Expert.t
  | Map of ('a Expert.t -> 'a Expert.t) * 'a t
  | List of 'a t list
  | Contramap : ('b t * ('a -> 'b)) -> 'a t
  | Pair : 'a t * unit t * 'b t -> ('a * 'b) t
  | Acc of
      { segment : 'a Acc.t Expert.t; elt : (module Integer.S with type t = 'a) }

type render_config = { interval : Mtime.span option; max_width : int option }

module Make (Clock : Mclock) (Platform : Platform.S) = struct
  module Expert = struct
    include Expert.Platform_dependent (Platform)
    include Expert
  end

  let compile : type a. a t -> config:render_config -> a Expert.t =
   fun t ~config ->
    let rec inner : type a. a t -> (unit -> bool) -> a Expert.t = function
      | Noop -> fun _ -> Expert.noop ()
      | Pair (a, sep, b) ->
          let a = inner a in
          let sep = inner sep in
          let b = inner b in
          fun should_update ->
            Expert.pair ~sep:(sep should_update) (a should_update)
              (b should_update)
      | Contramap (x, f) ->
          let x = inner x in
          fun y -> Expert.contramap ~f (x y)
      | Map (f, x) -> fun a -> f (inner x a)
      | List xs ->
          let xs = List.map inner xs in
          fun should_update ->
            Expert.array
              (List.map (fun f -> f should_update) xs |> Array.of_list)
      | Basic segment ->
          fun should_update ->
            Expert.conditional (fun _ -> should_update ()) @@ segment
      | Acc { segment; elt = (module Integer) } ->
          fun should_update ->
            Acc.wrap ~elt:(module Integer) ~clock:Clock.now ~should_update
            @@ segment
    in
    let inner = inner t in
    let segment =
      Expert.stateful (fun () ->
          let should_update =
            let state = { Timer.render_latest = Clock.now () } in
            Staged.prj
              (Timer.should_update ~clock:Clock.now ~interval:config.interval
                 state)
          in
          let x = ref true in
          Expert.contramap ~f:(fun a ->
              x := should_update ();
              a)
          @@ Expert.box_winsize ?max:config.max_width
          @@ inner (fun () -> !x))
    in
    segment

  (* Basic utilities for combining segments *)

  let noop () = Noop

  let const s =
    let len = String.length s and len_utf8 = String.Utf8.length s in
    let segment =
      Expert.theta ~width:len_utf8 (fun buf ->
          Line_buffer.add_substring buf s ~off:0 ~len)
    in
    Basic segment

  let const_fmt ~width pp =
    let segment =
      Expert.theta ~width (fun buf ->
          Line_buffer.with_ppf buf (fun ppf -> pp ppf))
    in
    Basic segment

  (* let _expert_of_pp ~width ~initial pp =
   *   Expert.alpha ~width ~initial (fun buf x ->
   *       Line_buffer.with_ppf buf (fun ppf -> pp ppf x)) *)

  let of_pp (type elt) ~elt ~width pp =
    let (module Integer : Integer.S with type t = elt) = elt in
    let segment =
      Expert.contramap ~f:Acc.total
      @@ Expert.alpha ~width ~initial:(`Val Integer.zero) (fun buf x ->
             let s = Format.asprintf "%a" pp x in
             Line_buffer.add_string buf s)
    in
    Acc { segment; elt }

  let of_printer (type elt) ~elt printer =
    let (module Integer : Integer.S with type t = elt) = elt in
    let pp = Staged.prj (Printer.to_line_printer printer) in
    let segment =
      Expert.contramap ~f:Acc.total
      @@ Expert.alpha ~width:(Printer.width printer) ~initial:(`Val Integer.zero)
           pp
    in
    Acc { segment; elt }

  let pair ?(sep = noop ()) a b = Pair (a, sep, b)

  let list ?(sep = const "  ") xs =
    let xs =
      ListLabels.filter_map xs ~f:(function Noop -> None | x -> Some x)
    in
    List (List.intersperse ~sep xs)

  let ( ++ ) a b = List [ a; b ]
  let using f x = Contramap (x, f)

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

  let spinner ?color ?stages () =
    let stages, width =
      match stages with
      | None -> ([| "⠁"; "⠂"; "⠄"; "⡀"; "⢀"; "⠠"; "⠐"; "⠈" |], 1)
      | Some [] -> Fmt.invalid_arg "spinner must have at least one stage"
      | Some (x :: xs as stages) ->
          let width = String.length (* UTF8 *) x in
          ListLabels.iter xs ~f:(fun x ->
              let width' = String.length x in
              if width <> width' then
                Fmt.invalid_arg
                  "spinner stages must have the same UTF-8 length. found %d \
                   and %d"
                  width width');
          (Array.of_list stages, width)
    in
    let stage_count = Array.length stages in
    Basic
      (Segment.stateful (fun () ->
           let tick = Staged.prj (modulo_counter stage_count) in
           Segment.theta ~width (fun buf ->
               with_color_opt color buf (fun () ->
                   Line_buffer.add_string buf stages.(tick ())))))

  let bytes = of_printer ~elt:(module Integer.Int) Units.Bytes.of_int
  let bytes_int64 = of_printer ~elt:(module Integer.Int64) Units.Bytes.of_int64

  let percentage_of (type elt) total
      (module Integer : Integer.S with type t = elt) =
    let printer =
      Printer.using Units.Percentage.of_float ~f:(fun x ->
          Integer.to_float x /. Integer.to_float total)
    in
    of_printer ~elt:(module Integer) printer

  let string =
    let segment =
      Segment.alpha_unsized ~initial:(`Val "") (fun ~width buf s ->
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

  let max (type elt) total (module Integer : Integer.S with type t = elt) =
    const (Integer.to_string total)

  let count (type elt) total (module Integer : Integer.S with type t = elt) =
    let total = Integer.to_string total in
    let width = String.length total in
    let segment =
      Expert.contramap ~f:Acc.total
      @@ Expert.alpha ~initial:(`Val Integer.zero) ~width (fun lb x ->
             let x = Integer.to_string x in
             for _ = String.length x to width - 1 do
               Line_buffer.add_char lb ' '
             done;
             Line_buffer.add_string lb x)
    in
    Acc { segment; elt = (module Integer) }

  let rpad sz t = Map (Expert.box_fixed ~pad:`right sz, t)

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

  let bar ?(style = `UTF8) ?color ?color_empty ?(width = `Expand) (type elt)
      ~total (module Integer : Integer.S with type t = elt) : elt t =
    let proportion x = Integer.to_float x /. Integer.to_float total in

    Acc
      { segment =
          Segment.contramap
            ~f:(fun x -> Acc.total x |> proportion)
            (match width with
            | `Fixed width ->
                if width < 3 then failwith "Not enough space for a progress bar";
                Segment.alpha ~width ~initial:(`Val 0.) (fun buf x ->
                    ignore
                      (bar ~style ~color ~color_empty (fun _ -> width) x buf
                        : int))
            | `Expand ->
                Segment.alpha_unsized ~initial:(`Val 0.) (fun ~width ppf x ->
                    bar ~style ~color ~color_empty width x ppf))
      ; elt = (module Integer)
      }

  (* TODO: common start time by using something like Acc *)
  (* let elapsed () =
   *   let print_time =
   *     Staged.prj (Printer.to_line_buffer Units.Duration.mm_ss_print)
   *   in
   *   let segment =
   *     Expert.contramap ~f: (fun acc ->
   *         let time = Mtime.span (Acc.render_start acc) (Clock.now ()) in
   *         Expert.theta ~width:5 (fun buf -> print_time buf time))
   *   in
   *   { segment; } *)

  let elapsed () =
    let print_time = Staged.prj (Printer.to_line_printer Units.Duration.mm_ss) in
    let segment =
      Expert.stateful (fun () ->
          let start_time = Clock.counter () in
          let pp buf = print_time buf (Clock.count start_time) in
          Expert.theta ~width:5 pp)
    in
    Basic segment

  let rate (type elt) pp_val (module Integer : Integer.S with type t = elt) :
      elt t =
    let segment =
      let pp_rate =
        let pp_val = Staged.prj (Printer.to_line_printer pp_val) in
        fun buf x ->
          pp_val buf x;
          Line_buffer.add_string buf "/s"
      in
      let width = Printer.width pp_val + 2 in
      Expert.contramap
        ~f:(Acc.ring_buffer >> Ring_buffer.rate_per_second >> Integer.to_float)
        (Expert.alpha ~width ~initial:(`Val 0.) pp_rate)
    in

    Acc { segment; elt = (module Integer) }

  let eta (type elt) ~total (module Integer : Integer.S with type t = elt) =
    let segment =
      let printer =
        let pp_val = Staged.prj (Printer.to_line_printer Units.Duration.mm_ss) in
        fun buf x ->
          Line_buffer.add_string buf "ETA: ";
          pp_val buf x
      in
      let width = Printer.width Units.Duration.mm_ss + 4 in
      let initial = `Val Mtime.Span.max_span in
      Expert.contramap
        ~f:(fun acc ->
          let per_second = Acc.ring_buffer acc |> Ring_buffer.rate_per_second in
          let acc = Acc.total acc in
          if per_second = Integer.zero then Mtime.Span.max_span
          else
            let todo = Integer.(to_float (sub total acc)) in
            Mtime.Span.of_uint64_ns
              (Int64.of_float
                 (todo /. Integer.to_float per_second *. 1_000_000_000.)))
        (Expert.alpha ~width ~initial printer)
    in

    Acc { elt = (module Integer); segment }
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
