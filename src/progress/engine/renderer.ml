(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(** The core of the progress bar rendering logic. Consumes the {!Line} DSL and
    emits rendering functions that put {!Ansi} escape codes in the right places. *)

include Renderer_intf
open! Import

type ('a, 'b) t = ('a, 'b) Segment_list.t

module Bar : sig
  type 'a t

  val create : 'a Segment_list.elt -> 'a t
  val contents : _ t -> string
  val update : unconditional:bool -> _ t -> unit -> int
  val report : 'a t -> 'a -> int
end = struct
  type 'a t =
    { line_buffer : Line_buffer.t
    ; update : unconditional:bool -> int
    ; report : 'a -> int
    }

  let create : type a. a Segment_list.elt -> a t =
   fun { segment } ->
    let s = Segment.compile segment in
    let line_buffer = Line_buffer.create ~size:80 in
    let report =
      let report = Staged.prj (Segment.report s) in
      fun (a : a) -> report line_buffer a
    in
    let update =
      let update = Staged.prj (Segment.update s) in
      fun ~unconditional -> update line_buffer ~unconditional
    in
    { line_buffer; report; update }

  let contents t = Line_buffer.contents t.line_buffer
  let update ~unconditional t () = t.update ~unconditional
  let report t = t.report
end

module Bar_list = struct
  type (_, _) t =
    | One : 'a Bar.t -> ('a reporter -> 'b, 'b) t
    | Many : 'a Bar.t list -> ('a reporter list -> 'b, 'b) t
    | Plus : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t

  let rec of_segments : type a b. (a, b) Segment_list.t -> (a, b) t = function
    | One elt -> One (Bar.create elt)
    | Many elts -> Many (List.map Bar.create elts)
    | Plus (xs, ys) -> Plus (of_segments xs, of_segments ys)

  let rec length : type a b. (a, b) t -> int = function
    | One _ -> 1
    | Many xs -> List.length xs
    | Plus (xs, ys) -> length xs + length ys
end

let make : type a b. a Segment.t -> (a reporter -> b, b) t =
 fun segment -> One { segment }

let make_list : type a b. a Segment.t list -> (a reporter list -> b, b) t =
 fun segments ->
  Many (List.map (fun segment -> Segment_list.{ segment }) segments)

module Uid : sig
  type t

  val create : unit -> t
  val equal : t -> t -> bool
end = struct
  type t = unit ref

  let create = ref
  let equal = ( == )
end

module Display : sig
  type t

  val create : config:Config.internal -> ('a, 'b) Bar_list.t -> t
  val uid : t -> Uid.t
  val rerender_bar : t -> idx:int -> width:int -> string -> unit
  val rerender_all : t -> unit
  val initial_render : t -> unit
  val handle_width_change : t -> int -> unit
  val interject_with : t -> (unit -> 'a) -> 'a
  val cleanup : t -> unit
  val finalize : t -> unit
end = struct
  type t =
    | E :
        { config : Config.internal
        ; bars : (_, _) Bar_list.t
        ; bar_count : int
        ; uid : Uid.t
        ; latest_widths : int array
        }
        -> t

  let create ~config bars =
    let uid = Uid.create () in
    let bar_count = Bar_list.length bars in
    E { config; bars; bar_count; uid; latest_widths = Array.make bar_count 0 }

  let uid (E { uid; _ }) = uid

  let rerender_line (E { config = { ppf; _ }; latest_widths; _ })
      ~width:new_width ~idx data =
    let old_width = latest_widths.(idx) in
    latest_widths.(idx) <- new_width;
    Format.pp_print_string ppf data;
    if new_width < old_width then Format.pp_print_string ppf Ansi.erase_line

  let rerender_all_from_top ~unconditional
      (E { config = { ppf; _ }; bars; bar_count; _ } as t) =
    let rec inner :
        type a b. on_right_spine:bool -> idx:int -> (a, b) Bar_list.t -> int =
     fun ~on_right_spine ~idx -> function
      | One bar ->
          let width = Bar.update ~unconditional bar () in
          rerender_line t ~width ~idx (Bar.contents bar);
          if on_right_spine then Format.fprintf ppf "\r%!"
          else Format.pp_force_newline ppf ();
          succ idx
      | Many [ bar ] ->
          let width = Bar.update ~unconditional bar () in
          rerender_line t ~width ~idx (Bar.contents bar);
          if on_right_spine then Format.fprintf ppf "\r%!"
          else Format.pp_force_newline ppf ();
          succ idx
      | Many (x :: (_ :: _ as xs)) ->
          let idx = inner ~on_right_spine:false ~idx (One x) in
          inner ~on_right_spine ~idx (Many xs)
      | Plus (xs, ys) ->
          let idx = inner ~on_right_spine:false ~idx xs in
          inner ~on_right_spine ~idx ys
      | Many [] -> assert false
     (* TODO: non-empty list *)
    in
    let seen = inner ~on_right_spine:true ~idx:0 bars in
    assert (seen = bar_count)

  let initial_render = rerender_all_from_top ~unconditional:true

  let rerender_bar (E { config = { ppf; _ }; bar_count; _ } as t) ~idx ~width
      data =
    let row = bar_count - idx - 1 in

    (* NOTE: we add an initial carriage return to avoid overflowing the line if
       the user has typed into the terminal between renders. *)
    Format.fprintf ppf "\r%a" Ansi.move_up row;
    rerender_line t ~idx ~width data;
    Format.fprintf ppf "%a\r%!" Ansi.move_down row

  let ceil_div x y = (x + y - 1) / y
  let overflow_rows ~old_width ~new_width = ceil_div old_width new_width - 1

  let handle_width_change
      (E { config = { ppf; _ }; latest_widths; bar_count; _ } as display) width
      =
    let overflows =
      ArrayLabels.fold_left latest_widths ~init:0 ~f:(fun a x ->
          a + overflow_rows ~old_width:x ~new_width:width)
    in
    let bottom_overflow =
      overflow_rows ~old_width:latest_widths.(bar_count - 1) ~new_width:width
    in
    Ansi.move_up ppf (overflows + bar_count - 1 - bottom_overflow);
    if overflows > 0 then Format.pp_print_string ppf Ansi.erase_display_suffix;
    rerender_all_from_top ~unconditional:true display

  let rerender_all (E { config = { ppf; _ }; bar_count; _ } as t) =
    Ansi.move_up ppf (bar_count - 1);
    rerender_all_from_top ~unconditional:false t

  let interject_with (E { config = { ppf; _ }; bar_count; _ } as t) f =
    Format.fprintf ppf "%a%s%!" Ansi.move_up (bar_count - 1) Ansi.erase_line;
    Fun.protect f ~finally:(fun () ->
        rerender_all_from_top ~unconditional:true t)

  let cleanup (E { config; _ }) =
    if config.hide_cursor then
      Format.fprintf config.ppf "\n%s%!" Ansi.show_cursor

  let finalize
      (E { config = { ppf; hide_cursor; persistent }; bar_count; _ } as display)
      =
    Ansi.move_up ppf (bar_count - 1);
    if persistent then (
      rerender_all_from_top ~unconditional:true display;
      Format.fprintf ppf "@,@]")
    else Format.pp_print_string ppf Ansi.erase_line;
    Format.fprintf ppf "%s%!" (if hide_cursor then Ansi.show_cursor else "")
end

module Make (Platform : Platform.S) = struct
  module Global : sig
    val active_display : unit -> Display.t option
    val set_active_exn : Display.t -> unit
    val set_inactive : unit -> unit
  end = struct
    type runtime =
      { (* Race conditions over these fields are not handled, but protection against
           concurrent usage is best-effort anyway. *)
        mutable active_display : Display.t option
      ; displaced_handlers : (int, Sys.signal_behavior) Hashtbl.t
      }

    let runtime =
      { active_display = None; displaced_handlers = Hashtbl.create 0 }

    let is_active () = Option.is_some runtime.active_display
    let active_display () = runtime.active_display
    let cleanup () = Option.iter Display.cleanup runtime.active_display

    let handle_width_change w =
      match (w, runtime.active_display) with
      | None, _ | _, None -> ()
      | Some w, Some display -> Display.handle_width_change display w

    let handle_signal code =
      cleanup ();
      match Hashtbl.find_opt runtime.displaced_handlers code with
      | Some (Signal_handle f) -> f code
      | Some Signal_default -> exit 100
      | Some Signal_ignore | None -> ()

    let init_handlers =
      lazy
        (at_exit cleanup;
         Platform.Width.set_changed_callback handle_width_change)

    let set_active_exn display =
      if is_active () then
        failwith "Can't run more than one progress bar renderer simultaneously";

      Lazy.force init_handlers;

      ListLabels.iter
        Sys.[ sigint; sigquit; sigterm; sigsegv ]
        ~f:(fun code ->
          let prev_handler = Sys.signal code (Signal_handle handle_signal) in
          (* Until the previous signal is added to the hashtable, there's a short
             period of time in which the {i previous} signal handler might be
             ignored. Not much we can do about that, unfortunately. *)
          Hashtbl.add runtime.displaced_handlers code prev_handler);
      runtime.active_display <- Some display

    let set_inactive () =
      Hashtbl.iter Sys.set_signal runtime.displaced_handlers;
      Hashtbl.clear runtime.displaced_handlers;
      runtime.active_display <- None
  end

  (* From the user's perspective, this is a handle to the currently-active
     display. We don't need one of those, since internally we always have a
     reference via [Global], but we keep track of identifiers anyway to ensure
     hygiene. *)
  type display = Uid.t

  let start :
        'a 'b. ?config:Config.t -> ('a, 'b) t -> ('a, 'b) Hlist.t * display =
   fun ?(config = Config.create ()) bars ->
    let config = Config.to_internal config in
    let bars = Bar_list.of_segments bars in
    let ppf = config.ppf in
    let display = Display.create ~config bars in
    Global.set_active_exn display;
    Format.pp_open_box ppf 0;
    if config.hide_cursor then Format.pp_print_string ppf Ansi.hide_cursor;
    Display.initial_render display;
    let rec inner : type a b. int -> (a, b) Bar_list.t -> int * (a, b) Hlist.t =
     fun seen -> function
      | One bar ->
          let reporter x =
            let width = Bar.report bar x in
            Display.rerender_bar display ~idx:seen ~width (Bar.contents bar)
          in
          (seen + 1, [ reporter ])
      | Many xs ->
          let reporters =
            ListLabels.mapi xs ~f:(fun i bar ->
                let reporter x =
                  let width = Bar.report bar x in
                  Display.rerender_bar display ~idx:(seen + i) ~width
                    (Bar.contents bar)
                in
                reporter)
          in
          (seen + List.length xs, [ reporters ])
      | Plus (left, right) ->
          let seen, left = inner seen left in
          let seen, right = inner seen right in
          (seen, Hlist.append left right)
    in
    let _, bars = inner 0 bars in
    (bars, Display.uid display)

  let finalize uid' =
    match Global.active_display () with
    | None -> failwith "Display already finalized"
    | Some display ->
        if not (Uid.equal (Display.uid display) uid') then
          failwith "Display already finalized";
        Display.finalize display;
        Global.set_inactive ()

  let with_reporters ?config t f =
    let reporters, display = start ?config t in
    Fun.protect
      (fun () -> Hlist.apply_all f reporters)
      ~finally:(fun () -> finalize display)

  let start ?config t =
    let hlist, disp = start ?config t in
    (Reporters.of_hlist hlist, disp)

  let interject_with : 'a. (unit -> 'a) -> 'a =
   fun f ->
    match Global.active_display () with
    | None -> f ()
    | Some d -> Display.interject_with d f

  let tick () =
    match Global.active_display () with
    | None -> ()
    | Some d -> Display.rerender_all d
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
