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

  val of_line : 'a Line_primitives.t -> 'a t
  val create : Config.t -> 'a Segment_list.elt -> 'a t
  val contents : _ t -> [ `Clean | `Dirty of string ]
  val update : unconditional:bool -> _ t -> unit -> int
  val report : 'a t -> 'a -> int
end = struct
  type 'a t =
    { line_buffer : Line_buffer.t
    ; update : unconditional:bool -> int
    ; report : 'a -> int
    }

  let of_line : type a. a Line_primitives.t -> a t =
   fun s ->
    let s = Line_primitives.compile s in
    let line_buffer = Line_buffer.create ~size:80 in
    let report =
      let report = Staged.prj (Line_primitives.report s) in
      fun (a : a) -> report line_buffer a
    in
    let update =
      let update = Staged.prj (Line_primitives.update s) in
      fun ~unconditional -> update line_buffer ~unconditional
    in
    { line_buffer; report; update }

  let create : type a. Config.t -> a Segment_list.elt -> a t =
   fun cfg segment -> of_line (segment cfg)

  let contents t = Line_buffer.contents t.line_buffer
  let update ~unconditional t () = t.update ~unconditional
  let report t = t.report
end

module Bar_list = struct
  type (_, _) t =
    | One : 'a Bar.t -> ('a reporter -> 'b, 'b) t
    | Many : 'a Bar.t list -> ('a reporter list -> 'b, 'b) t
    | Plus : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t

  let rec of_segments : type a b. Config.t -> (a, b) Segment_list.t -> (a, b) t
      =
   fun cfg -> function
    | One elt -> One (Bar.create cfg elt)
    | Many elts -> Many (List.map (fun x -> Bar.create cfg x) elts)
    | Plus (xs, ys) -> Plus (of_segments cfg xs, of_segments cfg ys)

  type 'b mapper = { f : 'a. 'a Bar.t -> 'b }

  let rec map : type a b c. (a, b) t -> f:c mapper -> c list =
   fun t ~f ->
    match t with
    | One b -> [ f.f b ]
    | Many bs -> List.map f.f bs
    | Plus (xs, ys) -> map ~f xs @ map ~f ys
end

module Display : sig
  type t

  module Uid : sig
    type t

    val equal : t -> t -> bool
  end

  val create : config:Config.t -> ('a, 'b) Bar_list.t -> t
  val uid : t -> Uid.t
  val initial_render : t -> unit

  val rerender_bar :
    t -> idx:int -> width:int -> [ `Clean | `Dirty of string ] -> unit

  val rerender_all : t -> unit
  val handle_width_change : t -> int -> unit
  val interject_with : t -> (unit -> 'a) -> 'a
  val add_bar : t -> _ Bar.t -> int
  val cleanup : t -> unit
  val finalize : t -> unit
  val config : t -> Config.t
end = struct
  module Uid = struct
    type t = unit ref

    let create = ref
    let equal = ( == )
  end

  type some_bar = E : _ Bar.t -> some_bar

  type t =
    { config : Config.t
    ; uid : Uid.t
    ; bars : (int, some_bar) Hashtbl.t
    ; latest_widths : (int, int) Hashtbl.t
    ; mutable bar_count : int (* *)
    }

  let config t = t.config

  let create ~config bars =
    let uid = Uid.create () in
    let bar_list = Bar_list.map ~f:{ f = (fun x -> E x) } bars in
    let bar_count = List.length bar_list in
    let bars = Hashtbl.create bar_count in
    let latest_widths = Hashtbl.create bar_count in

    ListLabels.iteri bar_list ~f:(fun i x ->
        Hashtbl.add bars i x;
        Hashtbl.add latest_widths i 0);

    { config; uid; bars; latest_widths; bar_count }

  let uid { uid; _ } = uid

  let rerender_line { config = { ppf; _ }; latest_widths; _ } ~width:new_width
      ~idx data =
    let old_width = Hashtbl.find latest_widths idx in
    Hashtbl.replace latest_widths idx new_width;
    Format.pp_print_string ppf data;
    if new_width < old_width then Format.pp_print_string ppf Ansi.erase_line

  let rerender_all_from_top ~unconditional
      ({ config = { ppf; _ }; bars; bar_count; _ } as t) =
    for idx = 0 to bar_count - 1 do
      let (E bar) = Hashtbl.find bars idx in
      let width = Bar.update ~unconditional bar () in
      (match Bar.contents bar with
      | `Clean -> ()
      | `Dirty contents -> rerender_line t ~width ~idx contents);

      match idx = bar_count - 1 with
      | false -> Format.pp_force_newline ppf ()
      | true -> Format.fprintf ppf "\r%!"
    done

  let initial_render = rerender_all_from_top ~unconditional:true

  let rerender_bar ({ config = { ppf; _ }; bar_count; _ } as t) ~idx ~width data
      =
    match data with
    | `Clean -> ()
    | `Dirty data ->
        let row = bar_count - idx - 1 in

        (* NOTE: we add an initial carriage return to avoid overflowing the line if
           the user has typed into the terminal between renders. *)
        Format.fprintf ppf "\r%a" Ansi.move_up row;
        rerender_line t ~idx ~width data;
        Format.fprintf ppf "%a\r%!" Ansi.move_down row

  let add_bar t bar =
    let i = t.bar_count in
    Hashtbl.add t.bars i (E bar);
    Hashtbl.add t.latest_widths i 0;
    t.bar_count <- succ t.bar_count;

    let width = Bar.update ~unconditional:true bar () in
    Format.pp_print_newline t.config.ppf ();
    (match Bar.contents bar with
    | `Clean -> ()
    | `Dirty contents -> rerender_line t ~width ~idx:i contents);
    Format.fprintf t.config.ppf "\r%!";
    i

  let ceil_div x y = (x + y - 1) / y
  let overflow_rows ~old_width ~new_width = ceil_div old_width new_width - 1

  let handle_width_change
      ({ config = { ppf; _ }; latest_widths; bar_count; _ } as display) width =
    let overflows =
      Hashtbl.fold
        (fun _ x a -> a + overflow_rows ~old_width:x ~new_width:width)
        latest_widths 0
    in
    let bottom_overflow =
      overflow_rows
        ~old_width:(Hashtbl.find latest_widths (bar_count - 1))
        ~new_width:width
    in
    Ansi.move_up ppf (overflows + bar_count - 1 - bottom_overflow);
    if overflows > 0 then Format.pp_print_string ppf Ansi.erase_display_suffix;
    rerender_all_from_top ~unconditional:true display

  let rerender_all ({ config = { ppf; _ }; bar_count; _ } as t) =
    Ansi.move_up ppf (bar_count - 1);
    rerender_all_from_top ~unconditional:false t

  let interject_with ({ config = { ppf; _ }; bar_count; _ } as t) f =
    Format.fprintf ppf "%a%s%!" Ansi.move_up (bar_count - 1) Ansi.erase_line;
    Fun.protect f ~finally:(fun () ->
        rerender_all_from_top ~unconditional:true t)

  let cleanup { config; _ } =
    if config.hide_cursor then
      Format.fprintf config.ppf "\n%s%!" Ansi.show_cursor

  let finalize
      ({ config = { ppf; hide_cursor; persistent; _ }; bar_count; _ } as
      display) =
    Ansi.move_up ppf (bar_count - 1);
    if persistent then (
      rerender_all_from_top ~unconditional:true display;
      Format.fprintf ppf "@,@]")
    else Format.pp_print_string ppf Ansi.erase_line;
    Format.fprintf ppf "%s%!" (if hide_cursor then Ansi.show_cursor else "")
end

module Make (Platform : Platform.S) = struct
  module Config = Config

  module Global : sig
    val active_display : unit -> Display.t option
    val find_display : Display.Uid.t -> (Display.t, [ `finalized ]) result
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

    let find_display uid =
      match active_display () with
      | None -> Error `finalized
      | Some display ->
          if not (Display.Uid.equal (Display.uid display) uid) then
            Error `finalized
          else Ok display

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
         Platform.Terminal_width.set_changed_callback handle_width_change)

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
     hygiene; perhaps someday we'll support multiple simultaneous renders. *)
  type display = Display.Uid.t

  let reporter_of_bar (type a) display ~idx (bar : a Bar.t) : a reporter =
   fun x ->
    let width = Bar.report bar x in
    Display.rerender_bar display ~idx ~width (Bar.contents bar)

  let start :
        'a 'b.    ?config:Config.user_supplied -> ('a, 'b) t
        -> ('a, 'b) Hlist.t * display =
   fun ?(config = Config.create ()) bars ->
    let config = Config.apply_defaults config in
    let bars = Bar_list.of_segments config bars in
    let ppf = config.ppf in
    let display = Display.create ~config bars in
    Global.set_active_exn display;
    Format.pp_open_box ppf 0;
    if config.hide_cursor then Format.pp_print_string ppf Ansi.hide_cursor;
    Display.initial_render display;
    let rec inner : type a b. int -> (a, b) Bar_list.t -> int * (a, b) Hlist.t =
     fun seen -> function
      | One bar ->
          let reporter = reporter_of_bar display ~idx:seen bar in
          (seen + 1, [ reporter ])
      | Many xs ->
          let reporters =
            ListLabels.mapi xs ~f:(fun i bar ->
                reporter_of_bar display ~idx:(seen + i) bar)
          in
          (seen + List.length xs, [ reporters ])
      | Plus (left, right) ->
          let seen, left = inner seen left in
          let seen, right = inner seen right in
          (seen, Hlist.append left right)
    in
    let _, bars = inner 0 bars in
    (bars, Display.uid display)

  let finalize uid =
    match Global.find_display uid with
    | Error `finalized -> failwith "Display already finalized"
    | Ok display ->
        Display.finalize display;
        Global.set_inactive ()

  let with_reporters ?config t f =
    let reporters, display = start ?config t in
    Fun.protect
      (fun () -> Hlist.apply_all f reporters)
      ~finally:(fun () -> finalize display)

  (** TODO: move this description elsewhere; it's not renderer related. *)

  module Line = Line.Platform_dependent (Platform)

  let with_reporter ?config b f =
    with_reporters ?config
      (Segment_list.One
         (fun config ->
           Line.compile b config (* TODO: stop calling everything 'compile' *)))
      f

  let start ?config t =
    let hlist, disp = start ?config t in
    (Reporters.of_hlist hlist, disp)

  let add_line uid line =
    match Global.find_display uid with
    | Error `finalized -> failwith "Cannot add a line to a finalised display"
    | Ok d ->
        let bar = Bar.of_line (Line.compile line (Display.config d)) in
        let idx = Display.add_bar d bar in
        reporter_of_bar d ~idx bar

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
