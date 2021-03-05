open! Utils
module Segment = Segment
module Units = Units

type 'a bar = {
  update : Format.formatter -> unit as 't;
  report : position:('t -> 't) -> Format.formatter -> 'a;
}

type 'a t = Pair : 'a t * 'b t -> ('a * 'b) t | Bar : 'a bar -> 'a t

let ( / ) top bottom = Pair (top, bottom)

let make : type a. init:a -> a Segment.t -> (a -> unit) t =
 fun ~init s ->
  let s = Segment.compile ~initial:init s in
  let report = Segment.report s in
  let update = Segment.update s in
  let report ~position =
    let buffer = Buffer.create 0 in
    let ppf_buf = Format.formatter_of_buffer buffer in
    (* Print via a buffer to avoid positioning to the correct row if there is
       nothing to print. *)
    fun ppf (a : a) ->
      report ppf_buf a;
      Format.pp_print_flush ppf_buf ();
      match Buffer.length buffer with
      | 0 -> ()
      | _ ->
          position
            (fun ppf -> Format.pp_print_string ppf (Buffer.contents buffer))
            ppf;
          Buffer.clear buffer
  in
  Bar { report; update }

module Internal = struct
  let counter ?prebar ~total ?(mode = `ASCII) ?message
      ?(pp : (int64, int64 Segment.t) Units.pp_fixed option) ?width
      ?(sampling_interval = 1) () =
    let open Segment in
    let proportion =
      let total = Int64.to_float total in
      fun i -> Int64.to_float i /. total
    in
    list
      (Option.fold ~none:[] message ~some:(fun s -> [ const s ])
      @ Option.fold ~none:[] pp ~some:(fun f -> [ f of_pp ])
      @ Option.fold ~none:[] prebar ~some:(fun s -> [ s ])
      @ [ bar ~mode proportion ])
    |> Option.fold width ~some:box_fixed ~none:(box_winsize ~fallback:80)
    |> periodic sampling_interval
    |> accumulator Int64.add 0L
    |> make ~init:0L
end

let counter = Internal.counter ?prebar:None

module Ansi = struct
  let show_cursor = "\x1b[?25h"
  let hide_cursor = "\x1b[?25l"
  let erase_line = "\x1b[K"
  let move_up ppf d = Format.fprintf ppf "\x1b[%dA" d
  let move_down ppf d = Format.fprintf ppf "\x1b[%dB" d
end

module Uid : sig
  type t

  val create : unit -> t
  val equal : t -> t -> bool
end = struct
  type t = unit ref

  let create = ref
  let equal = ( == )
end

module Display = struct
  type 'a bar_group = 'a t

  type t =
    | E : {
        ppf : Format.formatter;
        bars : _ bar_group;
        bar_count : int;
        uid : Uid.t;
      }
        -> t
end

module Global : sig
  val active_display : unit -> Display.t option
  val set_active_exn : Display.t -> unit
  val set_inactive : unit -> unit
end = struct
  type runtime = {
    (* Race conditions over these fields are not handled, but protection against
       concurrent usage is best-effort anyway. *)
    mutable active_display : Display.t option;
    displaced_handlers : (int, Sys.signal_behavior) Hashtbl.t;
  }

  let runtime = { active_display = None; displaced_handlers = Hashtbl.create 0 }
  let is_active () = Option.is_some runtime.active_display
  let active_display () = runtime.active_display

  let cleanup () =
    match runtime.active_display with
    | None -> ()
    | Some (E { ppf; _ }) -> Format.fprintf ppf "\n%s%!" Ansi.show_cursor

  let () = at_exit cleanup

  let handle_signal code =
    cleanup ();
    match Hashtbl.find_opt runtime.displaced_handlers code with
    | Some (Signal_handle f) -> f code
    | Some Signal_default -> exit 100
    | Some Signal_ignore | None -> ()

  let set_active_exn display =
    if is_active () then
      failwith "Can't run more then one progress bar renderer simultaneously";

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

let positioned_at ~row t ppf =
  if row = 0 then Format.fprintf ppf "%t\r%!" t
  else Format.fprintf ppf "%a%t%a\r%!" Ansi.move_up row t Ansi.move_down row

let rec count_bars : type a. a t -> int = function
  | Pair (a, b) -> count_bars a + count_bars b
  | Bar _ -> 1

let start ?(ppf = Format.err_formatter) bars =
  let bar_count = count_bars bars in
  let uid = Uid.create () in
  let display = Display.E { ppf; bars; bar_count; uid } in
  Global.set_active_exn display;
  Format.fprintf ppf "@[%s" Ansi.hide_cursor;
  let rec inner : type a. int -> a t -> int * a =
   fun seen -> function
    | Pair (a, b) ->
        let seen, a = inner seen a in
        let seen, b = inner seen b in
        (seen, (a, b))
    | Bar { report; update } ->
        update ppf;
        Format.fprintf ppf (if seen = bar_count - 1 then "\r%!" else "\n");
        ( seen + 1,
          report ~position:(positioned_at ~row:(bar_count - seen - 1)) ppf )
  in
  (inner 0 bars |> snd, uid)

let rerender_all (Display.E { ppf; bars; bar_count; _ }) =
  let rec inner : type a. int -> a t -> int =
   fun seen -> function
    | Pair (a, b) -> inner (inner seen a) b
    | Bar { update; _ } ->
        update ppf;
        Format.fprintf ppf (if seen = bar_count - 1 then "\r%!" else "\n");
        (* Unix.sleepf 1. ; *)
        seen + 1
  in
  ignore (inner 0 bars : int)

let finalise uid' =
  match Global.active_display () with
  | None -> failwith "Display already finalised"
  | Some (E { ppf; bars; uid; _ }) ->
      if not (Uid.equal uid uid') then failwith "Display already finalised";
      let rec inner : type a. row:int -> a t -> unit =
       fun ~row -> function
        | Bar { update; _ } -> positioned_at ~row update ppf
        | Pair (a, b) ->
            inner ~row:(succ row) a;
            inner ~row b
      in
      inner ~row:0 bars;
      Format.fprintf ppf "@,@]%s%!" Ansi.show_cursor;
      Global.set_inactive ()

let interject_with : 'a. (unit -> 'a) -> 'a =
 fun f ->
  match Global.active_display () with
  | None -> f ()
  | Some (E { ppf; bar_count; _ } as t) ->
      Format.fprintf ppf "%a%s%!" Ansi.move_up (bar_count - 1) Ansi.erase_line;
      let a = f () in
      rerender_all t;
      a

let with_reporters ?ppf t f =
  let reporters, display = start ?ppf t in
  let x = f reporters in
  finalise display;
  x
