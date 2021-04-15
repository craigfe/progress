type 'a t

(* Positive *)

val create : int -> 'a t
val of_list : 'a list -> 'a t
val of_array : 'a array -> 'a t

(* Negative *)

val length : _ t -> int
val insert : 'a t -> int -> 'a -> unit
val get_exn : 'a t -> int -> 'a

type 'a iter := 'a t -> f:('a -> unit) -> unit
type 'a iteri := 'a t -> f:(int -> 'a -> unit) -> unit

val iter : 'a iter
val iter_from : int -> 'a iter
val iteri : 'a iteri
val iteri_from : int -> 'a iteri
