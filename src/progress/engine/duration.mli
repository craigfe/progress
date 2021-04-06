(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

type t = Mtime.Span.t
(** The type of time durations. *)

val equal : t -> t -> bool
val compare : t -> t -> int

(** {2 Round values} *)

val nanosecond : t
val microsecond : t
val millisecond : t
val second : t
val minute : t
val hour : t
val day : t

(** {2 Convertors} *)

val of_ns : float -> t
val of_us : float -> t
val of_ms : float -> t
val of_sec : float -> t
val of_min : float -> t
val of_hour : float -> t
val of_day : float -> t

(** From integers: *)

val of_int_ms : int -> t
val of_int64_ms : int64 -> t
val of_int_sec : int -> t
val of_int64_sec : int64 -> t

(** Infix operators *)

val ( < ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( = ) : t -> t -> bool
val ( >= ) : t -> t -> bool
val ( > ) : t -> t -> bool

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
