open Mtime.Span

type nonrec t = t

let v = of_uint64_ns
let ( * ) = Int64.mul
let nanosecond = v 1L
let microsecond = v 1_000L
let millisecond = v 1_000_000L
let second = v 1_000_000_000L
let minute = v (60L * 1_000_000_000L)
let hour = v (60L * 60L * 1_000_000_000_000L)
let day = v (24L * 60L * 60L * 1_000_000_000_000L)
let of_ns x = v (Int64.of_float x)
let of_us x = v (Int64.of_float (x *. 1e3))
let of_ms x = v (Int64.of_float (x *. 1e6))
let of_sec x = v (Int64.of_float (x *. 1e9))
let of_int64_sec x = v (Int64.mul x 1_000_000_000L)
let of_int_sec x = of_int64_sec (Int64.of_int x)

let of_min =
  let f = 60. *. 1e9 in
  fun x -> v (Int64.of_float (x *. f))

let of_hour =
  let f = 60. *. 60. *. 1e9 in
  fun x -> v (Int64.of_float (x *. f))

let of_day =
  let f = 24. *. 60. *. 60. *. 1e9 in
  fun x -> v (Int64.of_float (x *. f))
