type bytes = int64

val kib : int -> bytes
val mib : int -> bytes
val gib : int -> bytes

type 'a pp := Format.formatter -> 'a -> unit
type 'a fixed_pp := 'a pp * int

val pp : bytes pp
val pp_fixed : bytes fixed_pp
