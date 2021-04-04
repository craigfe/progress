(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Line_intf
module Expert = Segment
open! Import
open Expert

type nonrec 'a t = 'a t

(* Basic utilities for combining segments *)

let const s =
  let len = String.length s and len_utf8 = String.Utf8.length s in
  theta ~width:len_utf8 (fun buf -> Line_buffer.add_substring buf s ~off:0 ~len)

let const_fmt ~width pp =
  theta ~width (fun buf -> Line_buffer.with_ppf buf (fun ppf -> pp ppf))

let of_pp ~width pp =
  alpha ~width (fun buf x -> Line_buffer.with_ppf buf (fun ppf -> pp ppf x))

let pair = pair
let list ?(sep = const "  ") = List.intersperse ~sep >> Array.of_list >> array
let ( ++ ) a b = array [| a; b |]
let using f x = contramap ~f x

(* Spinners *)

let with_style_opt ~style buf f =
  match style with
  | None -> f ()
  | Some s ->
      Line_buffer.add_style_code buf s;
      let a = f () in
      Line_buffer.add_style_code buf `None;
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
                "spinner stages must have the same UTF-8 length. found %d and \
                 %d"
                width width');
        (Array.of_list stages, width)
  in
  let stage_count = Array.length stages in
  stateful (fun () ->
      let tick = Staged.prj (modulo_counter stage_count) in
      theta ~width (fun buf ->
          with_style_opt buf ~style:color (fun () ->
              Line_buffer.add_string buf stages.(tick ()))))

let bytes = of_pp ~width:Units.Bytes.width Units.Bytes.of_int
let bytes_int64 = of_pp ~width:Units.Bytes.width Units.Bytes.of_int64
let percentage = of_pp ~width:Units.Percentage.width Units.Percentage.of_float

let string =
  alpha_unsized (fun ~width buf s ->
      let len = String.length s in
      if len <= width () then (
        Line_buffer.add_string buf s;
        len)
      else assert false)

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
  with_style_opt ~style:color buf (fun () ->
      for _ = 1 to filled do
        Line_buffer.add_string buf stages.(final_stage)
      done);
  let () =
    if filled <> bar_width then (
      let chunks = Float.to_int (squaresf *. Float.of_int final_stage) in
      let index = chunks - (filled * final_stage) in
      if index >= 0 && index < final_stage then
        with_style_opt ~style:color buf (fun () ->
            Line_buffer.add_string buf stages.(index));

      with_style_opt ~style:color_empty buf (fun () ->
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
  with_style_opt ~style:color buf (fun () ->
      for _ = 1 to filled do
        Line_buffer.add_char buf '#'
      done);
  with_style_opt ~style:color_empty buf (fun () ->
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

let bar ?(style = `UTF8) ?color ?color_empty ?(width = `Expand) f =
  contramap ~f
    (match width with
    | `Fixed width ->
        if width < 3 then failwith "Not enough space for a progress bar";
        alpha ~width (fun buf x ->
            ignore (bar ~style ~color ~color_empty (fun _ -> width) x buf : int))
    | `Expand ->
        alpha_unsized (fun ~width ppf x ->
            bar ~style ~color ~color_empty width x ppf))

module Time_sensitive (Clock : Mclock) = struct
  let elapsed () =
    Expert.stateful (fun () ->
        let start_time = Clock.counter () in
        let pp ppf = Units.Duration.mm_ss ppf (Clock.count start_time) in
        const_fmt ~width:5 pp)

  let rate (pp, width) =
    Expert.stateful (fun () ->
        let buf = Ring_buffer.create ~clock:Clock.now ~size:16 in
        let width = width + 2 in
        let pp ppf x = Fmt.pf ppf "%a/s" pp x in
        using
          (fun x ->
            Ring_buffer.record buf x;
            Int64.of_float (Ring_buffer.rate_per_second buf))
          (of_pp ~width pp))

  let eta total =
    Expert.stateful (fun () ->
        let buf = Ring_buffer.create ~clock:Clock.now ~size:16 in
        let pp ppf x = Fmt.pf ppf "ETA: %a" Units.Duration.mm_ss x in
        let width = 10 in
        let acc = ref 0L in
        using
          (fun x ->
            Ring_buffer.record buf x;
            acc := Int64.add !acc x;
            let per_second = Ring_buffer.rate_per_second buf in
            if per_second = 0. then Mtime.Span.max_span
            else
              let todo = Int64.(to_float (sub total !acc)) in
              Mtime.Span.of_uint64_ns
                (Int64.of_float (todo /. per_second *. 1_000_000_000.)))
          (of_pp ~width pp))

  let debounce interval s =
    Expert.stateful (fun () ->
        let latest = ref (Clock.now ()) in
        let should_update () =
          let now = Clock.now () in
          match Mtime.Span.compare (Mtime.span !latest now) interval >= 0 with
          | false -> false
          | true ->
              latest := now;
              true
        in
        Expert.conditional (fun _ -> should_update ()) s)

  type 'a accumulated = { acc : 'a; latest : 'a }

  let acc t = t.acc
  let latest t = t.latest

  let debounced_accumulator interval combine zero s =
    Expert.stateful (fun () ->
        let latest = ref (Clock.now ()) in
        let total = ref zero in
        let pending = ref zero in
        let should_update () =
          let now = Clock.now () in
          match Mtime.Span.compare (Mtime.span !latest now) interval >= 0 with
          | false -> false
          | true ->
              latest := now;
              true
        in
        using (fun a -> pending := combine !pending a)
        @@ Expert.conditional (fun _ -> should_update ())
        @@ using (fun () ->
               let acc = combine !total !pending in
               let latest = !pending in
               total := acc;
               pending := zero;
               { acc; latest })
        @@ s)
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
