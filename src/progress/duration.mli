type t = Mtime.Span.t
(** The type of time durations. *)

(** {2 Round values} *)

val nanosecond : t
val microsecond : t
val millisecond : t
val second : t
val minute : t
val hour : t
val day : t

(** {2 Convertors} **)

val of_ns : float -> t
val of_us : float -> t
val of_ms : float -> t
val of_sec : float -> t
val of_min : float -> t
val of_hour : float -> t
val of_day : float -> t

(** From integers: *)

val of_int_sec : int -> t
val of_int64_sec : int64 -> t
