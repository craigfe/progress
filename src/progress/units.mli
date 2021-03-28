type 'a pp := Format.formatter -> 'a -> unit

module Duration : sig
  val mm_ss : Mtime.Span.t pp
  (** Renders a time span in fixed-width [MM:SS] form. *)
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
