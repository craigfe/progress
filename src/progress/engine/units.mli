type 'a pp := Format.formatter -> 'a -> unit

module Duration : sig
  val mm_ss : Mtime.Span.t pp
  (** Renders a time span in fixed-width [MM:SS] form. *)

  val mm_ss_print : Mtime.Span.t Print.t
end

(** {2 Assorted pretty-printing utilities} *)

(** Prints a proportion as a percentage. e.g.

    {[
      0.      ↦  "  0%"
      0.42    ↦  " 42%"
      0.9999  ↦  " 99%"
      1.      ↦  "100%"
    ]}

    {b Note:} values will be clamped into the range [\[0., 1.\]]. *)
module Percentage : sig
  val of_float : float pp
  val width : int
end

(** Prints a numeric value as as a byte count. e.g.

    {[
      0                ↦  "   0.0 B  "
      999              ↦  " 999.0 B  "
      1024             ↦  "   1.0 KiB"
      1024 * 1023      ↦  "1023.0 KiB"
      1024 * 1024 - 1  ↦  "1023.9 KiB"
    ]}*)
module Bytes : sig
  val of_int : int pp
  val of_float : float pp
  val of_int64 : int64 pp
  val width : int

  (** Quick builders for base-2 byte counts *)

  val kib : int -> int64
  (** [kib n] is [n] kibibytes. *)

  val mib : int -> int64
  (** [mib n] is [n] mebibytes. *)

  val gib : int -> int64
  (** [gib n] is [n] gibibytes. *)

  val tib : int -> int64
  (** [tib n] is [n] tebibytes. *)

  val pib : int -> int64
  (** [pib n] is [n] pebibytes. *)
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
