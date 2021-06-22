(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open Mtime.Span

let ( ** ) = Int64.mul

type nonrec t = t

let equal = Mtime.Span.equal
let compare = Mtime.Span.compare
let compare_zero (f : int -> int -> _) a b = f (compare a b) 0 [@@inlined]
let ( + ) = Mtime.Span.add
let ( < ) = compare_zero ( < )
let ( <= ) = compare_zero ( <= )
let ( = ) = compare_zero ( = )
let ( >= ) = compare_zero ( >= )
let ( > ) = compare_zero ( > )
let v = of_uint64_ns
let nanosecond = v 1L
let microsecond = v 1_000L
let millisecond = v 1_000_000L
let second = v 1_000_000_000L
let minute = v (60L ** 1_000_000_000L)
let hour = v (60L ** 60L ** 1_000_000_000_000L)
let day = v (24L ** 60L ** 60L ** 1_000_000_000_000L)
let of_ns x = v (Int64.of_float x)
let of_us x = v (Int64.of_float (x *. 1e3))
let of_ms x = v (Int64.of_float (x *. 1e6))
let of_sec x = v (Int64.of_float (x *. 1e9))
let of_int64_ms x = v (x ** 1_000_000L)
let of_int_ms x = of_int64_ms (Int64.of_int x)
let of_int64_sec x = v (x ** 1_000_000_000L)
let of_int_sec x = of_int64_sec (Int64.of_int x)

let of_min =
  let f = 60. *. 1e9 in
  fun x -> v (Int64.of_float (x *. f))

let of_int64_min =
  let f = 60L ** 1_000_000_000L in
  fun x -> v (x ** f)

let of_int_min x = of_int64_min (Int64.of_int x)

let of_hour =
  let f = 60. *. 60. *. 1e9 in
  fun x -> v (Int64.of_float (x *. f))

let of_int64_hour =
  let f = 60L ** 60L ** 1_000_000_000L in
  fun x -> v (x ** f)

let of_int_hour x = of_int64_hour (Int64.of_int x)

let of_day =
  let f = 24. *. 60. *. 60. *. 1e9 in
  fun x -> v (Int64.of_float (x *. f))

module Of_int = struct
  let ms = of_int_ms
  let sec = of_int_sec
  let min = of_int_min
  let hour = of_int_hour
  let ( + ) = ( + )
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
