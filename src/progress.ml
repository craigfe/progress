open! Utils
module Segment = Segment

type 'a bar = {
  update : Format.formatter -> unit as 't;
  report : ('t -> 't) -> Format.formatter -> 'a;
}

type 'a t = Pair : 'a t * 'b t -> ('a * 'b) t | Bar : 'a bar -> 'a t

let ( / ) top bottom = Pair (top, bottom)

let of_segment : type a. a Segment.t -> init:a -> (a -> unit) t =
 fun s ~init:x ->
  let s = Segment.unstage s in
  let report wrap ppf x = wrap (Fun.flip (Segment.report s) x) ppf in
  Bar { report; update = (fun ppf -> Segment.report s ppf x) }

let counter ~mode ~total ~message ?pp ?width ?(sampling_interval = 1) () =
  let open Segment in
  let count_segment = match pp with Some p -> [ fmt p ] | None -> [] in
  let box = Option.fold width ~some:box_fixed ~none:box_winsize in
  let proportion =
    let total = Int64.to_float total in
    fun i -> Int64.to_float i /. total
  in
  list ([ const message ] @ count_segment @ [ time; bar ~mode proportion ])
  |> box
  |> accumulator Int64.add 0L
  |> periodic sampling_interval
  |> of_segment ~init:0L

type display = E : { ppf : Format.formatter; bars : _ t } -> display

let position row t ppf =
  if row = 0 then Format.fprintf ppf "%t\r%!" t
  else Format.fprintf ppf "\027[%dA%t\027[%dB\r%!" row t row

let start ?(ppf = Format.err_formatter) bars =
  Format.fprintf ppf "@[";
  let display = E { ppf; bars } in
  let rec inner : type a. row:int -> a t -> a =
   fun ~row -> function
    | Pair (a, b) ->
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
    | Pair (a, b) ->
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
