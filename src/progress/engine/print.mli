open! Import

type 'a t

val of_to_string : len:int -> ('a -> string) -> 'a t
val to_line_buffer : 'a t -> (Line_buffer.t -> 'a -> unit) Staged.t
