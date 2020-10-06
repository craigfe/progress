let fmt_noop _ _ = ()

type 'a bar = {
  update : Format.formatter -> unit as 't;
  report : ('t -> 't) -> Format.formatter -> 'a;
}

type 'a t = Join_y : 'a t * 'b t -> ('a * 'b) t | Bar : 'a bar -> 'a t

let ( <-> ) n s = Join_y (n, s)

let bar width percentage =
  let filled = Float.to_int (Float.of_int (width - 2) *. percentage /. 100.) in
  let not_filled = width - 2 - filled in
  "["
  ^ String.init filled (fun _ -> '#')
  ^ String.init not_filled (fun _ -> '.')
  ^ "]"

let default_width () =
  match Terminal_size.get_columns () with Some c -> c | None -> 80

let counter ~total ~message ?pp:(pp_count, count_width = (fmt_noop, 0))
    ?(width = default_width ()) ?(sampling_interval = 1) () =
  if sampling_interval <= 0 then
    Format.kasprintf invalid_arg
      "Invalid sampling_interval %d: must be a positive value" sampling_interval;
  let count = ref 0L in
  let percentage i =
    min (Float.trunc (Int64.to_float i *. 100. /. Int64.to_float total)) 100.
  in
  let start_time = Mtime_clock.counter () in
  let should_update : unit -> bool =
    let ticker = ref 0 in
    fun () ->
      ticker := (!ticker + 1) mod sampling_interval;
      !ticker = 0
  in
  let bar_width = width - String.length message - count_width - 16 in
  if bar_width < 3 then invalid_arg "Not enough space for a progress bar";
  let update ppf =
    let seconds = Mtime_clock.count start_time |> Mtime.Span.to_s in
    let percentage = percentage !count in
    (* if first then Format.pp_open_box ppf 0 else Format.fprintf ppf "\r"; *)
    Format.fprintf ppf "%s  %a  %02.0f:%02.0f  %s %3.0f%%%!" message pp_count
      !count (Float.div seconds 60.) (Float.rem seconds 60.)
      (bar bar_width percentage) percentage
  in
  let report wrapper =
    let update = wrapper update in
    fun ppf i ->
      count := Int64.add !count i;
      if should_update () then update ppf
  in
  Bar { update; report }

type display = E : { ppf : Format.formatter; bars : _ t } -> display

let position row t ppf =
  if row = 0 then Format.fprintf ppf "%t\r%!" t
  else Format.fprintf ppf "\027[%dA%t\027[%dB\r%!" row t row

let start ?(ppf = Format.err_formatter) bars =
  Format.fprintf ppf "@[";
  let display = E { ppf; bars } in
  let rec inner : type a. row:int -> a t -> a =
   fun ~row -> function
    | Join_y (a, b) ->
        let a = inner ~row:(succ row) a in
        let b = inner ~row b in
        (a, b)
    | Bar { report; update } ->
        update ppf;
        Format.fprintf ppf (if row = 0 then "\r%!" else "\n");
        report (position row) ppf
  in
  (inner ~row:0 bars, display)

let finalise (E { ppf; bars }) =
  let rec inner : type a. row:int -> a t -> unit =
   fun ~row -> function
    | Bar { update; _ } -> position row update ppf
    | Join_y (a, b) ->
        inner ~row:(succ row) a;
        inner ~row b
  in
  inner ~row:0 bars;
  Format.fprintf ppf "@,@]%!"

let with_display ?ppf t f =
  let reporters, display = start ?ppf t in
  let x = f reporters in
  finalise display;
  x

module Bytes = Bytes

let bytes = Bytes.pp_fixed
