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
val add_style_code : t -> Ansi.style -> unit

val contents : t -> string
(** Reset the write head to the start of the buffer and return a copy of the
    intervening contents. *)

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
