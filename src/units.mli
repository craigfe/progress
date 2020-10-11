type 'a pp := Format.formatter -> 'a -> unit
type 'a pp_fixed := 'a pp * int

val bytes : int64 pp_fixed
(** Prints a 64-bit integer as a byte count. e.g.

    {[
      0L                  ↦  "   0.0 B  "
      999L                ↦  " 999.0 B  "
      1024L               ↦  "   1.0 KiB"
      1024L * 1023L       ↦  "1023.0 KiB"
      1024L * 1024L - 1L  ↦  "1023.9 KiB"
    ]}*)

[@@@ocamlformat "parse-docstrings = false"]

val percentage : float pp_fixed
(** Prints a proportion as a percentage. e.g.

    {[
      0.      ↦  "  0%"
      0.42    ↦  " 42%"
      0.9999  ↦  " 99%"
      1.      ↦  "100%"
    ]}

    {b Note:} values will be clamped into the range [\[0., 1.\]]. *)

[@@@ocamlformat "parse-docstrings = true"]

(** {2 Assorted pretty-printing utilities} *)

module Percentage : sig
  type t := float

  val pp : t pp

  val pp_fixed : t pp_fixed
  (** {!pp_fixed} is {!pp} with left-padding added to achieve fixed size. *)
end

module Bytes : sig
  type t := int64

  val pp : t pp

  val pp_fixed : t pp_fixed
  (** {!pp_fixed} is {!pp} with padding added to achieve fixed size. *)

  (** Quick builders for base-2 byte counts *)

  val kib : int -> t
  (** [kib n] is [n] kibibytes. *)

  val mib : int -> t
  (** [mib n] is [n] mebibytes. *)

  val gib : int -> t
  (** [gib n] is [n] gibibytes. *)

  val tib : int -> t
  (** [tib n] is [n] tebibytes. *)

  val pib : int -> t
  (** [pib n] is [n] pebibytes. *)
end
