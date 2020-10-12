type 'a pp := Format.formatter -> 'a -> unit

type ('a, 'b) pp_fixed = (width:int -> 'a pp -> 'b) -> 'b
(** A fixed-width pretty printer. Intended to be used to create progress bar
    segments, e.g. {!bytes} {!Segment.of_pp} is a segment that pretty-prints
    values as byte counts. *)

val bytes : (int64, _) pp_fixed
(** Prints a 64-bit integer as a byte count. e.g.

    {[
      0L                  ↦  "   0.0 B  "
      999L                ↦  " 999.0 B  "
      1024L               ↦  "   1.0 KiB"
      1024L * 1023L       ↦  "1023.0 KiB"
      1024L * 1024L - 1L  ↦  "1023.9 KiB"
    ]}*)

[@@@ocamlformat "parse-docstrings = false"]

val percentage : (float, _) pp_fixed
(** Prints a proportion as a percentage. e.g.

    {[
      0.      ↦  "  0%"
      0.42    ↦  " 42%"
      0.9999  ↦  " 99%"
      1.      ↦  "100%"
    ]}

    {b Note:} values will be clamped into the range [\[0., 1.\]]. *)

[@@@ocamlformat "parse-docstrings = true"]

val seconds : (Mtime.Span.t, _) pp_fixed
(** Renders a time span in fixed-width [MM:SS] form. *)

(** {2 Assorted pretty-printing utilities} *)

module Percentage : sig
  type t := float

  val pp : t pp

  val pp_fixed : t pp * int
  (** {!pp_fixed} is {!pp} with left-padding added to achieve fixed size. *)
end

module Bytes : sig
  type t := int64

  val pp : t pp

  val pp_fixed : t pp * int
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
