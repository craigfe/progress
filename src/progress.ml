type 'a fmt = Format.formatter -> 'a -> unit
type 'a fixed_width_fmt = 'a fmt * int

let fmt_noop _ _ = ()

(* module HList = struct
 *   type 'a t = [] : unit t | ( :: ) : ('a * 'b t) -> ('a * 'b) t
 * end *)

(** Pretty-printer for byte counts *)
let pp_bytes : int64 fixed_width_fmt =
  (* Round down to the nearest 0.1 *)
  let trunc f = Float.trunc (f *. 10.) /. 10. in
  let pp ppf i =
    match Int64.to_float i with
    | n when n < 1024. -> Format.fprintf ppf "%6.1f B  " (trunc n)
    | n when n < 1024. ** 2. ->
        Format.fprintf ppf "%6.1f KiB" (trunc (n /. 1024.))
    | n when n < 1024. ** 3. ->
        Format.fprintf ppf "%6.1f MiB" (trunc (n /. (1024. ** 2.)))
    | n -> Format.fprintf ppf "%6.1f GiB" (trunc (n /. (1024. ** 3.)))
  in
  (pp, 10)

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

let default_columns () =
  match Terminal_size.get_columns () with Some c -> c | None -> 80

let counter ~total ~sampling_interval ?(columns = default_columns ()) ~message
    ?pp_count:(pp_count, count_width = (fmt_noop, 0)) () =
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
  let bar_width = columns - String.length message - count_width - 16 in
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
