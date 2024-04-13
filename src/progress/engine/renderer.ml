(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(** The core of the progress bar rendering logic. Consumes the {!Line} DSL and
    emits rendering functions that put {!Ansi} escape codes in the right places. *)

include Renderer_intf
open! Import
module Bar_id = Unique_id ()

(* TODO: this module should probably be inlined with the
   [Line_primitives.Compiled.t] types: since those values only ever correspond to
   exactly one [Bar_renderer], and both are responsible for state management. *)
module Bar_renderer : sig
  type 'a t

  type contents =
    { width : int; data : [ `Clean of string | `Dirty of string ] }

  val create : 'a Line_primitives.t -> 'a t
  val update : unconditional:bool -> _ t -> unit -> contents
  val report : 'a t -> 'a -> contents
  val tick : _ t -> contents
  val finalise : _ t -> contents
  val id : _ t -> Bar_id.t
end = struct
  type 'a t =
    { line_buffer : Line_buffer.t
    ; update : unconditional:bool -> int
    ; report : 'a -> int
    ; finalise : unit -> int
    ; tick : unit -> int
    ; id : Bar_id.t
    ; mutable finalised : bool
    }

  type contents =
    { width : int; data : [ `Clean of string | `Dirty of string ] }

  (* TODO: get rid of all this boilerplate *)

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
    let finalise =
      let finalise = Staged.prj (Line_primitives.finalise s) in
      fun () -> finalise line_buffer
    in
    let tick =
      let tick = Staged.prj (Line_primitives.tick s) in
      fun () -> tick line_buffer
    in
    let id = Bar_id.create () in
    { line_buffer; report; update; finalise; tick; id; finalised = false }

  let finalise t =
    let width = t.finalise () in
    let data = Line_buffer.contents t.line_buffer in
    t.finalised <- true;
    { width; data }

  let tick t =
    if t.finalised then finalise t
    else
      let width = t.tick () in
      let data = Line_buffer.contents t.line_buffer in
      { width; data }

  let update ~unconditional t () =
    (* We continue to render even once the bar has been finalised in order to
       account for terminal width changes. *)
    if t.finalised then finalise t
    else
      let width = t.update ~unconditional in
      let data = Line_buffer.contents t.line_buffer in
      { width; data }

  let report t x =
    if t.finalised then finalise t
    else
      let width = t.report x in
      let data = Line_buffer.contents t.line_buffer in
      { width; data }

  let id t = t.id
end

module Bar_list = struct
  include Multi.Hlist (Bar_renderer)
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
  val tick : t -> unit
  val handle_width_change : t -> int -> unit
  val pause : t -> unit
  val resume : t -> unit
  val interject_with : t -> (unit -> 'a) -> 'a
  val cleanup : t -> unit
  val finalise : t -> unit

  (* Bar-specific functions *)
  val add_line : ?above:int -> t -> _ Bar_renderer.t -> unit
  val remove_line : t -> Bar_id.t -> unit
  val rerender_line : t -> Bar_id.t -> Bar_renderer.contents -> unit
  val finalise_line : t -> Bar_id.t -> unit
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
      let f i = function
        | None -> None
        | Some renderer -> Some (E { renderer; latest_width = 0; position = i })
      in
      Bar_list.mapi bars ~f:{ f } |> Vector.of_list ~dummy:None
    in
    let bar_count = Bar_list.length bars in
    let bars = Hashtbl.create bar_count in
    Vector.iter rows
      ~f:
        (Option.iter (fun (E { renderer; _ } as t) ->
             Hashtbl.add bars ~key:(Bar_renderer.id renderer) ~data:t));

    { config; uid; bars; rows }

  let uid { uid; _ } = uid

  (* Terminals generally don't wrap blank suffixes of lines (e.g. [" \n"] is
     equivalent to ["\n"]), so we should account for this when estimating the
     width at which a given line will be wrapped. *)
  let get_blank_suffix_length =
    let rec aux str = function
      | -1 -> -1
      | i -> ( match str.[i] with ' ' -> aux str (i - 1) | _ -> i)
    in
    fun str ->
      let last_index = String.length str - 1 in
      last_index - aux str last_index

  let rerender_line_and_advance { config = { ppf; _ }; _ } (E bar) new_width
      data =
    let old_width = bar.latest_width in
    bar.latest_width <- new_width - get_blank_suffix_length data;
    Format.pp_print_string ppf data;
    if new_width < old_width then
      Format.pp_print_string ppf Terminal.Ansi.erase_line

  let rerender_all_from_top ~stage ~starting_at ~unconditional
      ({ config = { ppf; _ }; rows; _ } as t) =
    let total_rows = Vector.length rows in
    Vector.iteri_from starting_at rows ~f:(fun idx slot ->
        let is_last = idx = total_rows - 1 in
        let () =
          match slot with
          | None -> Format.fprintf ppf "%s" Terminal.Ansi.erase_line
          | Some (E bar) -> (
              let ({ width; data } : Bar_renderer.contents) =
                match stage with
                | `update -> Bar_renderer.update ~unconditional bar.renderer ()
                | `tick -> Bar_renderer.tick bar.renderer
                | `finalise -> Bar_renderer.finalise bar.renderer
              in
              match data with
              | `Clean _ when not unconditional -> ()
              | `Clean contents | `Dirty contents ->
                  rerender_line_and_advance t (E bar) width contents)
        in
        match is_last with
        | false -> Format.pp_force_newline ppf ()
        | true -> Format.fprintf ppf "\r%!")

  let initial_render =
    rerender_all_from_top ~stage:`update ~starting_at:0 ~unconditional:true

  let get_bar_exn ~msg bars uid =
    let exception Finalised of string in
    match Hashtbl.find bars uid with
    | x -> x
    | exception Not_found -> raise (Finalised msg)

  let rerender_line ({ config = { ppf; _ }; bars; rows; _ } as t) uid
      ({ width; data } : Bar_renderer.contents) =
    let (E bar) = get_bar_exn ~msg:"Can't render to finalised bar" bars uid in
    match data with
    | `Clean _ -> ()
    | `Dirty data ->
        let distance_from_base = Vector.length rows - bar.position - 1 in

        (* NOTE: we add an initial carriage return to avoid overflowing the line if
           the user has typed into the terminal between renders. *)
        Format.fprintf ppf "\r%a" Terminal.Ansi.move_up distance_from_base;
        rerender_line_and_advance t (E bar) width data;
        Format.fprintf ppf "%a\r%!" Terminal.Ansi.move_down distance_from_base

  let finalise_line t uid =
    let (E bar) = get_bar_exn ~msg:"Bar already finalised" t.bars uid in
    let contents = Bar_renderer.finalise bar.renderer in
    rerender_line t uid contents;
    Hashtbl.remove t.bars uid

  let add_line ?(above = 0) t renderer =
    let position = Vector.length t.rows - above in
    let key = Bar_renderer.id renderer in
    let bar = E { renderer; latest_width = 0; position } in
    Hashtbl.add t.bars ~key ~data:bar;

    Vector.insert t.rows position (Some bar);
    Vector.iteri_from (position + 1) t.rows ~f:(fun i -> function
      | None -> () | Some (E bar) -> bar.position <- i);

    (* The cursor is now one line above the bottom. Move to the correct starting
       position for a re-render of the affected suffix of the display. *)
    Format.pp_force_newline t.config.ppf ();
    Terminal.Ansi.move_up t.config.ppf above;
    rerender_all_from_top ~stage:`update ~starting_at:position
      ~unconditional:true t

  let remove_line t key =
    let (E { position; _ }) =
      match Hashtbl.find_opt t.bars key with
      | Some bar -> bar
      | None -> (
          (* This can either mean that the line has already been finalised, or
             that this key is unknown. *)
          match
            Vector.find_map t.rows ~f:(function
              | None -> None
              | Some (E bar) as some_bar ->
                  if Bar_id.equal key (Bar_renderer.id bar.renderer) then
                    some_bar
                  else None)
          with
          | Some bar -> bar
          | None -> failwith "No such line in display")
    in
    if Hashtbl.mem t.bars key then Hashtbl.remove t.bars key;
    Vector.remove t.rows position;
    Vector.iteri_from position t.rows ~f:(fun i -> function
      | None -> () | Some (E bar) -> bar.position <- i);

    (* The cursor is now one line below the bottom. Move to the correct starting
       position for a re-render of the affected suffix of the display. *)
    Format.pp_print_string t.config.ppf Terminal.Ansi.erase_display_suffix;
    Terminal.Ansi.move_up t.config.ppf 1;
    Terminal.Ansi.move_up t.config.ppf (Vector.length t.rows - position - 1);
    rerender_all_from_top ~stage:`update ~starting_at:position
      ~unconditional:true t

  let ceil_div x y = (x + y - 1) / y

  let overflow_rows ~old_width ~new_width =
    max 0 (ceil_div old_width new_width - 1)

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
    Terminal.Ansi.move_up ppf move_up;
    if overflows > 0 then
      Format.pp_print_string ppf Terminal.Ansi.erase_display_suffix;
    rerender_all_from_top ~stage:`update ~starting_at:0 ~unconditional:true
      display

  let tick ({ config = { ppf; _ }; rows; _ } as t) =
    Terminal.Ansi.move_up ppf (Vector.length rows - 1);
    rerender_all_from_top ~stage:`tick ~starting_at:0 ~unconditional:false t

  let pause { config = { ppf; _ }; rows; _ } =
    Format.fprintf ppf "%s%!" Terminal.Ansi.erase_line;
    for _ = 1 to Vector.length rows - 1 do
      Format.fprintf ppf "%a%s%!" Terminal.Ansi.move_up 1
        Terminal.Ansi.erase_line
    done

  let resume t =
    rerender_all_from_top ~stage:`update ~starting_at:0 ~unconditional:true t

  let interject_with t f =
    pause t;
    Fun.protect f ~finally:(fun () -> resume t)

  let cleanup { config; _ } =
    if config.hide_cursor then
      Format.fprintf config.ppf "\n%s%!" Terminal.Ansi.show_cursor

  let finalise
      ({ config = { ppf; hide_cursor; persistent; _ }; rows; _ } as display) =
    Terminal.Ansi.move_up ppf (Vector.length rows - 1);
    if persistent then (
      rerender_all_from_top ~stage:`finalise ~starting_at:0 ~unconditional:true
        display;
      Format.fprintf ppf "@,@]")
    else Format.pp_print_string ppf Terminal.Ansi.erase_display_suffix;
    Format.fprintf ppf "%s%!"
      (if hide_cursor then Terminal.Ansi.show_cursor else "")
end

module Reporter = struct
  type 'a t =
    { uid : Bar_id.t
    ; display : Display.Unique_id.t
    ; update : unconditional:bool -> unit
    ; report : 'a -> unit
    }

  let report t = t.report

  type (_, _) list =
    | [] : ('a, 'a) list
    | ( :: ) : 'a * ('b, 'c) list -> ('a -> 'b, 'c) list
end

module Make (Platform : Platform.S) = struct
  module Config = Config
  module Line = Line.Make (Platform)

  module Global : sig
    val active_display : unit -> Display.t option
    val find_display : Display.Unique_id.t -> (Display.t, [ `finalised ]) result
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
      | None -> Error `finalised
      | Some display ->
          if not (Display.Unique_id.equal (Display.uid display) uid) then
            Error `finalised
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

    let signals =
      let open Sys in
      [ sigint; sigterm; sigsegv ] @ if win32 then [] else [ sigquit ]

    let set_active_exn display =
      if is_active () then
        failwith "Can't run more than one progress bar renderer simultaneously";

      Lazy.force init_handlers;

      ListLabels.iter signals ~f:(fun code ->
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

  let instrument_logs_reporter : Logs.reporter -> Logs.reporter =
    let wrap_msgf : 'a 'b. ('a, 'b) Logs.msgf -> ('a, 'b) Logs.msgf =
     fun msgf construction -> interject_with (fun () -> msgf construction)
    in
    fun r ->
      { report =
          (fun src level ~over k f -> r.report src level ~over k (wrap_msgf f))
      }

  let logs_reporter ?pp_header ?app ?dst () =
    instrument_logs_reporter (Logs_fmt.reporter ?pp_header ?app ?dst ())

  module Reporter = struct
    include Reporter

    let finalise (t : _ t) =
      match Global.find_display t.display with
      | Error `finalised -> failwith "Display already finalised"
      | Ok display -> Display.finalise_line display t.uid
  end

  module Reporters = struct
    type nonrec ('a, 'b) t = ('a, 'b) Reporter.list

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
     fun ?(config = Config.v ()) bars ->
      let config = Config.apply_defaults config in
      let bars = Bar_list.of_multi config bars in
      let ppf = config.ppf in
      let display = Display.create ~config bars in
      Global.set_active_exn display;
      Format.pp_open_box ppf 0;
      if config.hide_cursor then
        Format.pp_print_string ppf Terminal.Ansi.hide_cursor;
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
      | Error `finalised -> failwith "Cannot add a line to a finalised display"
      | Ok d ->
          let bar =
            Bar_renderer.create (Line.to_primitive (Display.config d) line)
          in
          let () = Display.add_line ?above d bar in
          let uid = Bar_renderer.id bar in
          let report = reporter_of_bar d bar in
          let update = updater_of_bar d bar in
          { display = Display.uid d; uid; report; update }

    let remove_line t (reporter : _ Reporter.t) =
      match Global.find_display t.uid with
      | Error `finalised ->
          failwith
            "Cannot remove a line from a display that is already finalised"
      | Ok d -> Display.remove_line d reporter.uid

    let finalise t =
      match Global.find_display t.uid with
      | Error `finalised -> failwith "Display already finalised"
      | Ok display ->
          Display.finalise display;
          Global.set_inactive ()

    let tick t =
      match Global.find_display t.uid with
      | Error `finalised -> ()
      | Ok d -> Display.tick d

    let reporters t = t.initial_reporters

    let pause t =
      match Global.find_display t.uid with
      | Error `finalised -> failwith "Cannot pause a finalised display"
      | Ok d -> Display.pause d

    let resume t =
      match Global.find_display t.uid with
      | Error `finalised -> failwith "Cannot pause a finalised display"
      | Ok d -> Display.resume d
  end

  let with_reporters ?config t f =
    let display = Display.start ?config t in
    Fun.protect
      (fun () -> Reporters.apply_all f (Display.reporters display))
      ~finally:(fun () -> Display.finalise display)

  let with_reporter ?config b f = with_reporters ?config (Multi.line b) f
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
