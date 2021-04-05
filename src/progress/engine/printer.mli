(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

type -'a t

val create : to_string:('a -> string) -> string_len:int -> pp:'a pp -> 'a t
val using : f:('b -> 'a) -> 'a t -> 'b t
val of_to_string : len:int -> ('a -> string) -> 'a t

(** Consuming printers *)

val to_pp : 'a t -> Format.formatter -> 'a -> unit
val to_line_printer : 'a t -> (Line_buffer.t -> 'a -> unit) Staged.t
val width : _ t -> int
