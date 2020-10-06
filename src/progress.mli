type 'a t
(** The type of progress bars with reporting functions of type ['a]. *)

val counter :
  total:int64 ->
  sampling_interval:int ->
  ?columns:int ->
  message:string ->
  ?pp:(Format.formatter -> int64 -> unit) * int ->
  unit ->
  (int64 -> unit) t
(** Renders a progress bar of the form:

    [<msg> <count> MM:SS \[########..............................\] XX%] *)

val ( <-> ) : 'a t -> 'b t -> ('a * 'b) t
(** Stack progres bars vertically. [a <-> b] is a display bar with [a] stacked
    on top of [b]. *)

type display

val with_display : ?ppf:Format.formatter -> 'a t -> ('a -> 'b) -> 'b
(** Render a set of progress bars inside a continuation.

    @param ppf Defaults to {!Format.err_formatter} *)

val start : ?ppf:Format.formatter -> 'a t -> 'a * display
val finalise : display -> unit

module Bytes = Bytes

val bytes : (Format.formatter -> int64 -> unit) * int
