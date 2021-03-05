open! Utils
module Segment = Segment

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

type display = E : { ppf : Format.formatter; bars : _ t } -> display

module Ansi = struct
  let show_cursor = "\x1b[?25h"
  let hide_cursor = "\x1b[?25l"
  let move_up ppf d = Format.fprintf ppf "\x1b[%dA" d
  let move_down ppf d = Format.fprintf ppf "\x1b[%dB" d
end

module Global : sig
  val is_active : unit -> bool
  val set_active : Format.formatter -> unit
  val set_inactive : unit -> unit
end = struct
  type runtime = {
    (* Race conditions over this field are not handled, but protection against
       concurrent usage is best-effort anyway. *)
    mutable ppf : Format.formatter option;
    displaced_handlers : (int, Sys.signal_behavior) Hashtbl.t;
  }

  let runtime = { ppf = None; displaced_handlers = Hashtbl.create 0 }
  let is_active () = Option.is_some runtime.ppf

  let cleanup () =
    match runtime.ppf with
    | None -> ()
    | Some ppf -> Format.fprintf ppf "\n%s%!" Ansi.show_cursor

  let () = at_exit cleanup

  let handle_signal code =
    cleanup ();
    match Hashtbl.find_opt runtime.displaced_handlers code with
    | Some (Signal_handle f) -> f code
    | Some Signal_default -> exit 100
    | Some Signal_ignore | None -> ()

  let set_active ppf =
    ListLabels.iter
      Sys.[ sigint; sigquit; sigterm; sigsegv ]
      ~f:(fun code ->
        let prev_handler = Sys.signal code (Signal_handle handle_signal) in
        (* Until the previous signal is added to the hashtable, there's a short
           period of time in which the {i previous} signal handler might be
           ignored. Not much we can do about that, unfortunately. *)
        Hashtbl.add runtime.displaced_handlers code prev_handler);
    runtime.ppf <- Some ppf

  let set_inactive () =
    Hashtbl.iter Sys.set_signal runtime.displaced_handlers;
    Hashtbl.clear runtime.displaced_handlers;
    runtime.ppf <- None
end

let positioned_at ~row t ppf =
  if row = 0 then Format.fprintf ppf "%t\r%!" t
  else Format.fprintf ppf "%a%t%a\r%!" Ansi.move_up row t Ansi.move_down row

let rec count_bars : type a. a t -> int = function
  | Pair (a, b) -> count_bars a + count_bars b
  | Bar _ -> 1

let start ?(ppf = Format.err_formatter) bars =
  if Global.is_active () then
    failwith "Can't run more then one progress bar renderer simultaneously";
  Global.set_active ppf;
  Format.fprintf ppf "@[%s" Ansi.hide_cursor;
  let display = E { ppf; bars } in
  let count =
    (* It's convenient to traverse left-to-right, so that we can do the bar
       initialisations in the correct order. However, the report functions need
       to know how many bars are _beneath_ them, so we do a total count
       first. *)
    count_bars bars
  in
  let rec inner : type a. int -> a t -> int * a =
   fun seen -> function
    | Pair (a, b) ->
        let seen, a = inner seen a in
        let seen, b = inner seen b in
        (seen, (a, b))
    | Bar { report; update } ->
        update ppf;
        Format.fprintf ppf (if seen = count - 1 then "\r%!" else "\n");
        (seen + 1, report ~position:(positioned_at ~row:(count - seen - 1)) ppf)
  in
  (inner 0 bars |> snd, display)

let finalise (E { ppf; bars }) =
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

let with_reporters ?ppf t f =
  let reporters, display = start ?ppf t in
  let x = f reporters in
  finalise display;
  x

module Units = Units
