type t
(** The type of ring buffers. *)

val create : size:int -> t

val record : t -> int64 -> unit
(** Add a value to the ring buffer. *)

val rate_per_second : t -> float
(** Estimate the rate of change of recorded values per second. *)
