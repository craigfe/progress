type 'a t
(** The type of ring buffers. *)

val create :
     clock:(unit -> Mtime.t)
  -> size:int
  -> elt:(module Integer.S with type t = 'a)
  -> 'a t

val record : 'a t -> 'a -> unit
(** Add a value to the ring buffer. *)

val rate_per_second : 'a t -> 'a
(** Estimate the rate of change of recorded values per second. *)
