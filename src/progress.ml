open! Utils
module Segment = Segment

type 'a bar = {
  update : Format.formatter -> unit as 't;
  report : position:('t -> 't) -> Format.formatter -> 'a;
}

type 'a t = Pair : 'a t * 'b t -> ('a * 'b) t | Bar : 'a bar -> 'a t

let ( / ) top bottom = Pair (top, bottom)

let of_segment : type a. a Segment.t -> init:a -> (a -> unit) t =
 fun s ~init ->
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
      match Buffer.length buffer with
      | 0 -> ()
      | _ ->
          position
            (fun ppf -> Format.pp_print_string ppf (Buffer.contents buffer))
            ppf;
          Buffer.clear buffer
  in
  Bar { report; update }

let counter ~mode ~total ~message ?pp ?width ?(sampling_interval = 1) () =
  let open Segment in
  let count_segment =
    match pp with Some (p, width) -> [ fmt ~width p ] | None -> []
  in
  let proportion =
    let total = Int64.to_float total in
    fun i -> Int64.to_float i /. total
  in
  list ([ const message ] @ count_segment @ [ time; bar ~mode proportion ])
  |> Option.fold width ~some:box_fixed ~none:box_winsize
  |> periodic sampling_interval
  |> accumulator Int64.add 0L
  |> of_segment ~init:0L

type display = E : { ppf : Format.formatter; bars : _ t } -> display

let positioned_at ~row t ppf =
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
        report ~position:(positioned_at ~row) ppf
  in
  (inner ~row:0 bars, display)

let finalise (E { ppf; bars }) =
  let rec inner : type a. row:int -> a t -> unit =
   fun ~row -> function
    | Bar { update; _ } -> positioned_at ~row update ppf
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
