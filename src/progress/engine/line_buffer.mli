(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

type t
(** A line buffer is a variant of [Stdlib.Buffer] that supports {i skipping}
    some section of the underlying bytestring when doing a write pass. *)

val create : size:int -> t
(** Create a line buffer with the given initial size. *)

val with_ppf : t -> (Format.formatter -> 'a) -> 'a
(** [with_ppf buf f] gives a view of [buf] as a formatter to [f] (and then
    flushes the formatter to [buf]). *)

val add_char : t -> char -> unit
val add_string : t -> string -> unit
val add_substring : t -> string -> off:int -> len:int -> unit
val add_line_buffer : dst:t -> src:t -> unit

val lift_write :
     len:int
  -> write:('a -> into:bytes -> pos:int -> unit)
  -> (t -> 'a -> unit) Staged.t

val contents : t -> string
(** Reset the write head to the start of the buffer and return a copy of the
    intervening contents. *)

val reset : t -> unit

type mark

val current_position : t -> mark
(** Get a mark of the current write head in the buffer. *)

module Span : sig
  type t

  val empty : t
  val between_marks : mark -> mark -> t
  val pp : Format.formatter -> t -> unit
end

val skip : t -> Span.t -> unit
(** Advance over a given span in the buffer. *)

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
