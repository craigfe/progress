(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(** The core of the progress bar rendering logic. Consumes the {!Line} DSL and
    emits rendering functions that put {!Ansi} escape codes in the right places. *)

include Renderer_intf
open! Import
module Bar_id = Unique_id ()

module Bar_renderer : sig
  type 'a t
  type contents = { width : int; data : [ `Clean | `Dirty of string ] }

  val create : 'a Line_primitives.t -> 'a t
  val update : unconditional:bool -> _ t -> unit -> contents
  val report : 'a t -> 'a -> contents
  val id : _ t -> Bar_id.t
end = struct
  type 'a t =
    { line_buffer : Line_buffer.t
    ; update : unconditional:bool -> int
    ; report : 'a -> int
    ; id : Bar_id.t
    }

  type contents = { width : int; data : [ `Clean | `Dirty of string ] }

  let create : type a. a Line_primitives.t -> a t =
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
    let id = Bar_id.create () in
    { line_buffer; report; update; id }

  let update ~unconditional t () =
    let width = t.update ~unconditional in
    let data = Line_buffer.contents t.line_buffer in
    { width; data }

  let report t x =
    let width = t.report x in
    let data = Line_buffer.contents t.line_buffer in
    { width; data }

  let id t = t.id
end

module Bar_list = struct
  include Multi.Hlist (Bar_renderer)
end

module Reporter = struct
  type 'a t =
    | Noop
    | Active of
        { uid : Bar_id.t
        ; update : unconditional:bool -> unit
        ; report : 'a -> unit
        }

  let noop = Noop
  let push = function Noop -> Fun.const () | Active { report; _ } -> report
end

module Display : sig
  type t

  module Unique_id : Eq

  val create : config:Config.t -> (_, _) Bar_list.t -> t
  (** Initialise a display with a given set of bar renderers. *)

  (* Immutable state *)
  val uid : t -> Unique_id.t
  val config : t -> Config.t

  (* Lifecycle management *)
  val initial_render : t -> unit
  val rerender_all : t -> unit
  val handle_width_change : t -> int -> unit
  val interject_with : t -> (unit -> 'a) -> 'a
  val cleanup : t -> unit
  val finalize : t -> unit

  (* Bar-specific functions *)
  val add_line : ?above:int -> t -> _ Bar_renderer.t -> unit
  val rerender_line : t -> Bar_id.t -> Bar_renderer.contents -> unit
  val finalize_line : t -> Bar_id.t -> unit
end = struct
  module Unique_id = Unique_id ()

  type some_bar =
    | E :
        { renderer : _ Bar_renderer.t
        ; mutable latest_width : int
        ; mutable position : int
        }
        -> some_bar

  type t =
    { config : Config.t
    ; uid : Unique_id.t
    ; bars : (Bar_id.t, some_bar) Hashtbl.t
    ; rows : some_bar option Vector.t
    }

  let config t = t.config

  let create ~config bars =
    let uid = Unique_id.create () in
    let rows =
      Bar_list.mapi bars
        ~f:
          { f =
              (fun i -> function
                | None -> None
                | Some renderer ->
                    Some (E { renderer; latest_width = 0; position = i }))
          }
      |> Vector.of_list
    in
    let bar_count = Bar_list.length bars in
    let bars = Hashtbl.create bar_count in
    Vector.iter rows ~f:(function
      | None -> ()
      | Some (E { renderer; _ } as t) ->
          Hashtbl.add bars ~key:(Bar_renderer.id renderer) ~data:t);

    { config; uid; bars; rows }

  let uid { uid; _ } = uid

  let rerender_line_and_advance { config = { ppf; _ }; _ } (E bar) new_width
      data =
    let old_width = bar.latest_width in
    bar.latest_width <- new_width;
    Format.pp_print_string ppf data;
    if new_width < old_width then Format.pp_print_string ppf Ansi.erase_line

  let rerender_all_from_top ~starting_at ~unconditional
      ({ config = { ppf; _ }; rows; _ } as t) =
    let total_rows = Vector.length rows in
    Vector.iteri_from starting_at rows ~f:(fun idx -> function
      | None -> Fmt.pf ppf "%s@\n" Ansi.erase_line
      | Some (E bar) -> (
          let ({ width; data } : Bar_renderer.contents) =
            Bar_renderer.update ~unconditional bar.renderer ()
          in
          (match data with
          | `Clean -> ()
          | `Dirty contents ->
              rerender_line_and_advance t (E bar) width contents);

          let is_last = idx = total_rows - 1 in
          match is_last with
          | false -> Format.pp_force_newline ppf ()
          | true -> Format.fprintf ppf "\r%!"))

  let initial_render = rerender_all_from_top ~starting_at:0 ~unconditional:true

  let rerender_line ({ config = { ppf; _ }; bars; rows; _ } as t) uid
      ({ width; data } : Bar_renderer.contents) =
    let (E bar) = Hashtbl.find bars uid in
    match data with
    | `Clean -> ()
    | `Dirty data ->
        let distance_from_base = Vector.length rows - bar.position - 1 in

        (* Format.printf "uid = %a; positions: %a@." Bar_id.pp
         *   (Bar_renderer.id bar.renderer)
         *   pp_map t.bar_by_position; *)

        (* NOTE: we add an initial carriage return to avoid overflowing the line if
           the user has typed into the terminal between renders. *)
        Format.fprintf ppf "\r%a" Ansi.move_up distance_from_base;
        rerender_line_and_advance t (E bar) width data;
        Format.fprintf ppf "%a\r%!" Ansi.move_down distance_from_base

  let finalize_line ({ config = { ppf; _ }; bars; rows; _ } as t) uid =
    (* Format.printf "finalising bar with id: %a@." Bar_id.pp uid; *)
    let (E bar) = Hashtbl.find bars uid in
    let ({ width; data } : Bar_renderer.contents) =
      Bar_renderer.update bar.renderer ~unconditional:true ()
    in
    match data with
    | `Clean -> ()
    | `Dirty data ->
        let distance_from_base = Vector.length rows - bar.position - 1 in

        (* NOTE: we add an initial carriage return to avoid overflowing the line if
           the user has typed into the terminal between renders. *)
        Format.fprintf ppf "\r%a" Ansi.move_up distance_from_base;
        rerender_line_and_advance t (E bar) width data;
        Format.fprintf ppf "%a\r%!" Ansi.move_down distance_from_base

  let add_line ?(above = 0) t renderer =
    let position = Vector.length t.rows - above in
    let key = Bar_renderer.id renderer in
    let bar = E { renderer; latest_width = 0; position } in
    Hashtbl.add t.bars ~key ~data:bar;

    Vector.insert t.rows position (Some bar);

    (* The cursor is now one line above the bottom. Move to the correct starting
       position for a re-render of the affected suffix of the display. *)
    Format.pp_force_newline t.config.ppf ();
    Ansi.move_up t.config.ppf above;
    rerender_all_from_top ~starting_at:position ~unconditional:true t

  let ceil_div x y = (x + y - 1) / y
  let overflow_rows ~old_width ~new_width = ceil_div old_width new_width - 1

  let handle_width_change ({ config = { ppf; _ }; rows; _ } as display)
      new_width =
    let row_count = Vector.length rows in
    let latest_widths =
      Array.init row_count ~f:(fun i ->
          Vector.get_exn rows i
          |> Option.fold ~none:0 ~some:(fun (E t) -> t.latest_width))
    in
    let overflows =
      Array.fold_left latest_widths ~init:0 ~f:(fun a old_width ->
          a + overflow_rows ~old_width ~new_width)
    in
    let bottom_overflow =
      overflow_rows ~old_width:latest_widths.(row_count - 1) ~new_width
    in
    let move_up = overflows + row_count - 1 - bottom_overflow in
    Ansi.move_up ppf move_up;

    (* let out =
     *   Fmt.str
     *     "{ new_width = %d; rows = %d; latest_widths = %a; overflows = %d; \
     *      bottom_overflow = %d; move_up = %d }"
     *     new_width row_count (Fmt.Dump.array Fmt.int) latest_widths overflows
     *     bottom_overflow move_up
     * in
     * Printf.fprintf oc "%s\n%!" out; *)
    if overflows > 0 then Format.pp_print_string ppf Ansi.erase_display_suffix;
    rerender_all_from_top ~starting_at:0 ~unconditional:true display

  let rerender_all ({ config = { ppf; _ }; rows; _ } as t) =
    Ansi.move_up ppf (Vector.length rows - 1);
    rerender_all_from_top ~starting_at:0 ~unconditional:false t

  let interject_with ({ config = { ppf; _ }; rows; _ } as t) f =
    Format.fprintf ppf "%a%s%!" Ansi.move_up
      (Vector.length rows - 1)
      Ansi.erase_line;
    Fun.protect f ~finally:(fun () ->
        rerender_all_from_top ~starting_at:0 ~unconditional:true t)

  let cleanup { config; _ } =
    if config.hide_cursor then
      Format.fprintf config.ppf "\n%s%!" Ansi.show_cursor

  let finalize
      ({ config = { ppf; hide_cursor; persistent; _ }; rows; _ } as display) =
    Ansi.move_up ppf (Vector.length rows - 1);
    if persistent then (
      rerender_all_from_top ~starting_at:0 ~unconditional:true display;
      Format.fprintf ppf "@,@]")
    else Format.pp_print_string ppf Ansi.erase_line;
    Format.fprintf ppf "%s%!" (if hide_cursor then Ansi.show_cursor else "")
end

module Make (Platform : Platform.S) = struct
  module Config = Config
  module Reporter = Reporter
  module Line = Line.Make (Platform)

  module Global : sig
    val active_display : unit -> Display.t option
    val find_display : Display.Unique_id.t -> (Display.t, [ `finalized ]) result
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
          if not (Display.Unique_id.equal (Display.uid display) uid) then
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
          Hashtbl.add runtime.displaced_handlers ~key:code ~data:prev_handler);
      runtime.active_display <- Some display

    let set_inactive () =
      Hashtbl.iter runtime.displaced_handlers ~f:(fun ~key ~data ->
          Sys.set_signal key data);
      Hashtbl.clear runtime.displaced_handlers;
      runtime.active_display <- None
  end

  let reporter_of_bar (type a) display (bar : a Bar_renderer.t) : a reporter =
   fun x ->
    Bar_renderer.report bar x
    |> Display.rerender_line display (Bar_renderer.id bar)

  let updater_of_bar display bar ~unconditional =
    Bar_renderer.update ~unconditional bar ()
    |> Display.rerender_line display (Bar_renderer.id bar)

  module Bar_list = struct
    include Bar_list

    let rec of_multi : type a b. Config.t -> (a, b) Multi.t -> (a, b) Bar_list.t
        =
      let elt cfg x = Bar_renderer.create (Line.to_primitive cfg x) in
      fun cfg -> function
        | Zero -> Zero
        | One x -> One (elt cfg x)
        | Many xs -> Many (List.map xs ~f:(fun x -> elt cfg x))
        | Plus (xs, ys) -> Plus (of_multi cfg xs, of_multi cfg ys)
  end

  module Hlist = struct
    (* ['a] and ['b] correspond to parameters of [Bar_list.t]. *)
    type (_, _) t =
      | [] : ('a, 'a) t
      | ( :: ) : 'a * ('b, 'c) t -> ('a -> 'b, 'c) t

    let rec append : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
     fun xs ys -> match xs with [] -> ys | x :: xs -> x :: append xs ys
  end

  let interject_with : 'a. (unit -> 'a) -> 'a =
   fun f ->
    match Global.active_display () with
    | None -> f ()
    | Some d -> Display.interject_with d f

  module Reporters = struct
    type (_, _) t =
      | [] : ('a, 'a) t
      | ( :: ) : 'a * ('b, 'c) t -> ('a -> 'b, 'c) t

    let rec apply_all : type a b. a -> (a, b) t -> b =
     fun f -> function [] -> f | x :: xs -> apply_all (f x) xs

    let rec of_hlist : type a b. (a, b) Hlist.t -> (a, b) t = function
      | [] -> []
      | x :: xs -> x :: of_hlist xs
  end

  module Display = struct
    type ('a, 'b) t =
      { uid : Display.Unique_id.t; initial_reporters : ('a, 'b) Reporters.t }

    let start :
        type a b. ?config:Config.user_supplied -> (a, b) Multi.t -> (a, b) t =
     fun ?(config = Config.create ()) bars ->
      let config = Config.apply_defaults config in
      let bars = Bar_list.of_multi config bars in
      let ppf = config.ppf in
      let display = Display.create ~config bars in
      Global.set_active_exn display;
      Format.pp_open_box ppf 0;
      if config.hide_cursor then Format.pp_print_string ppf Ansi.hide_cursor;
      Display.initial_render display;
      let rec inner : type a b. (a, b) Bar_list.t -> (a, b) Hlist.t = function
        | Zero -> []
        | One bar -> [ reporter_of_bar display bar ]
        | Many xs ->
            [ ListLabels.map xs ~f:(fun bar -> reporter_of_bar display bar) ]
        | Plus (left, right) ->
            let left = inner left in
            let right = inner right in
            Hlist.append left right
      in
      let bars = inner bars in
      { uid = Display.uid display; initial_reporters = Reporters.of_hlist bars }

    let add_line ?above t line : _ Reporter.t =
      match Global.find_display t.uid with
      | Error `finalized -> failwith "Cannot add a line to a finalised display"
      | Ok d ->
          let bar =
            Bar_renderer.create (Line.to_primitive (Display.config d) line)
          in
          let () = Display.add_line ?above d bar in
          let uid = Bar_renderer.id bar in
          let report = reporter_of_bar d bar in
          let update = updater_of_bar d bar in
          Active { uid; report; update }

    let finalize_line t r =
      match Global.find_display t.uid with
      | Error `finalized -> failwith "Display already finalized"
      | Ok display -> (
          match r with
          | Reporter.Noop -> ()
          | Active { uid; _ } -> Display.finalize_line display uid)
    (* TODO: use [bar] / [line] consistently *)

    let finalize t =
      match Global.find_display t.uid with
      | Error `finalized -> failwith "Display already finalized"
      | Ok display ->
          Display.finalize display;
          Global.set_inactive ()

    let tick t =
      match Global.find_display t.uid with
      | Error `finalized -> ()
      | Ok d -> Display.rerender_all d

    let reporters t = t.initial_reporters
  end

  let with_reporters ?config t f =
    let display = Display.start ?config t in
    Fun.protect
      (fun () -> Reporters.apply_all f (Display.reporters display))
      ~finally:(fun () -> Display.finalize display)

  let with_reporter ?config b f = with_reporters ?config (Multi.v b) f
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
