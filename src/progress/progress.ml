open! Utils
module Segment = Segment
module Units = Units

module Bar = struct
  type 'a t = {
    update : Format.formatter -> unit as 't;
    report : position:('t -> 't) -> Format.formatter -> 'a;
  }

  module List = struct
    type 'a bar = 'a t

    type (_, _) t =
      | [] : ('a, 'a) t
      | ( :: ) : 'a bar * ('b, 'c) t -> ('a -> 'b, 'c) t

    let rec append : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
     fun x y -> match x with [] -> y | x :: xs -> x :: append xs y

    let rec length : type a b. (a, b) t -> int = function
      | _ :: xs -> 1 + length xs
      | [] -> 0
  end
end

type 'a reporter = 'a -> unit
type ('a, 'b) t = ('a, 'b) Bar.List.t

let ( / ) top bottom = Bar.List.append top bottom

let make : type a b. init:a -> a Segment.t -> ((a -> unit) -> b, b) Bar.List.t =
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
  Bar.[ { report; update } ]

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

let counter ~total ?mode ?message ?pp ?width ?sampling_interval () =
  Internal.counter ?prebar:None ~total ?mode ?message ?pp ?width
    ?sampling_interval ()

module Ansi = struct
  let show_cursor = "\x1b[?25h"
  let hide_cursor = "\x1b[?25l"
  let erase_line = "\x1b[K"
  let move_up ppf = function 0 -> () | n -> Format.fprintf ppf "\x1b[%dA" n
  let move_down ppf = function 0 -> () | n -> Format.fprintf ppf "\x1b[%dB" n
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

module Config = struct
  type t = { ppf : Format.formatter; hide_cursor : bool }

  let create ?(ppf = Format.err_formatter) ?(hide_cursor = true) () =
    { ppf; hide_cursor }
end

module Display = struct
  type t =
    | E : {
        config : Config.t;
        bars : (_, _) Bar.List.t;
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
    | Some (E { config = { hide_cursor = true; ppf }; _ }) ->
        Format.fprintf ppf "\n%s%!" Ansi.show_cursor
    | _ -> ()

  let () = at_exit cleanup

  let handle_signal code =
    cleanup ();
    match Hashtbl.find_opt runtime.displaced_handlers code with
    | Some (Signal_handle f) -> f code
    | Some Signal_default -> exit 100
    | Some Signal_ignore | None -> ()

  let set_active_exn display =
    if is_active () then
      failwith "Can't run more than one progress bar renderer simultaneously";

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
  Format.fprintf ppf "%a%t%a\r%!" Ansi.move_up row t Ansi.move_down row

let rerender_all (Display.E { config = { ppf; _ }; bars; _ }) =
  let rec inner : type a b. (a, b) Bar.List.t -> unit = function
    | [] -> ()
    | [ { update; _ } ] ->
        update ppf;
        Format.fprintf ppf "\r%!"
    | { update; _ } :: bs ->
        update ppf;
        Format.pp_force_newline ppf ();
        inner bs
  in
  inner bars

module Hlist = struct
  (* ['a] and ['b] correspond to parameters of [Bar.List.t]. *)
  type (_, _) t =
    | [] : ('a, 'a) t
    | ( :: ) : 'a * ('b, 'c) t -> ('a -> 'b, 'c) t

  let rec apply_all : type a b. a -> (a, b) t -> b =
   fun f -> function [] -> f | x :: xs -> apply_all (f x) xs
end

let start : 'a 'b. ?config:Config.t -> ('a, 'b) t -> ('a, 'b) Hlist.t * display
    =
 fun ?(config = Config.create ()) bars ->
  let ppf = config.ppf in
  let bar_count = Bar.List.length bars in
  let uid = Uid.create () in
  let display = Display.E { config; bars; bar_count; uid } in
  Global.set_active_exn display;
  Format.pp_open_box ppf 0;
  if config.hide_cursor then Format.pp_print_string ppf Ansi.hide_cursor;
  rerender_all display;
  let rec inner : type a b. int -> (a, b) t -> (a, b) Hlist.t =
   fun seen -> function
    | [] -> []
    | { report; _ } :: bs ->
        let reporter =
          report ~position:(positioned_at ~row:(bar_count - seen - 1)) ppf
        in
        reporter :: inner (succ seen) bs
  in
  (inner 0 bars, uid)

let finalise uid' =
  match Global.active_display () with
  | None -> failwith "Display already finalised"
  | Some (E { config = { ppf; hide_cursor }; uid; bar_count; _ } as display) ->
      if not (Uid.equal uid uid') then failwith "Display already finalised";
      Ansi.move_up ppf (bar_count - 1);
      rerender_all display;
      if hide_cursor then Format.fprintf ppf "@,@]%s%!" Ansi.show_cursor;
      Global.set_inactive ()

let interject_with : 'a. (unit -> 'a) -> 'a =
 fun f ->
  match Global.active_display () with
  | None -> f ()
  | Some (E { config = { ppf; _ }; bar_count; _ } as t) ->
      Format.fprintf ppf "%a%s%!" Ansi.move_up (bar_count - 1) Ansi.erase_line;
      Fun.protect f ~finally:(fun () -> rerender_all t)

let with_reporters ?config t f =
  let reporters, display = start ?config t in
  Fun.protect
    (fun () -> Hlist.apply_all f reporters)
    ~finally:(fun () -> finalise display)

(* Present a slightly simpler type of heterogeneous lists to the user for use
   with [start], since they don't need to concatenate them. *)
module Reporters = struct
  type _ t = [] : unit t | ( :: ) : 'a * 'b t -> ('a -> 'b) t

  let rec of_hlist : type a. (a, unit) Hlist.t -> a t = function
    | [] -> []
    | x :: xs -> x :: of_hlist xs
end

let start ?config t =
  let hlist, reporters = start ?config t in
  (Reporters.of_hlist hlist, reporters)
