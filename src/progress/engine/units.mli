(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

module Duration : sig
  val mm_ss : Duration.t Printer.t
  (** Prints a time span in fixed-width [MM:SS] form. *)

  val hh_mm_ss : Duration.t Printer.t
  (** Prints a time span in fixed-width [HH:MM:SS] form. *)
end

(** Prints a proportion as a percentage.

    {[
      0.      ↦  "  0%"
      0.42    ↦  " 42%"
      0.9999  ↦  " 99%"
      1.      ↦  "100%"
    ]}

    {b Note:} values will be clamped into the range [\[0., 1.\]]. *)
module Percentage : sig
  val of_float : float Printer.t
end

(** Prints a numeric value as as a byte count.

    {[
      0                ↦  "   0.0 B  "
      999              ↦  " 999.0 B  "
      1024             ↦  "   1.0 KiB"
      1024 * 1023      ↦  "1023.0 KiB"
      1024 * 1024 - 1  ↦  "1023.9 KiB"
    ]}*)
module Bytes : sig
  val generic : (module Integer.S with type t = 't) -> 't Printer.t
  val of_int : int Printer.t
  val of_float : float Printer.t
  val of_int64 : int64 Printer.t

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

  val pp_int63 : int63 Fmt.t
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
