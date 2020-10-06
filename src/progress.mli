(** A library for displaying progress bars, including support for multiple bars
    displayed at once. Start by {{!description} describing} of a set of progress
    bars, then begin {{!rendering} rendering} them to get access to their
    respective reporting functions. *)

type 'a pp := Format.formatter -> 'a -> unit
type 'a pp_fixed := 'a pp * int

(** {1 Description} *)

type 'a t
(** The type of progress bars with reporting functions of type ['a]. The
    reporting functions are supplied when beginning the {{!rendering} rendering}
    process. *)

val counter :
  total:int64 ->
  message:string ->
  ?pp:int64 pp_fixed ->
  ?width:int ->
  ?sampling_interval:int ->
  unit ->
  (int64 -> unit) t
(** [counter ~total ~message ()] is a progress bar of the form:

    [<message> <count?> MM:SS \[########..............................\] XX%]

    where each reported value contributes cumulatively towards an eventual total
    of [total]. Optional parameters are as follows:

    - [?pp] is used to pretty-print the [<count>] segment, if passed. For
      example, {!bytes} can be used for totals measured in bytes. The default is
      to not display this segment.

    - [?width] is the width of the bar in columns. Defaults to the width of
      [stdout], if it is a terminal.

    - [?sampling_interval] specifies the number of reported values that must be
      passed for each update of the display (not including the update during
      finalisation, which always occurs). This is useful when progress is being
      reported from a hot-loop, where the cost of re-displaying the progress bar
      is non-negligible. The default value is [1], meaning that all updates are
      displayed immediately. *)

val ( <-> ) : 'a t -> 'b t -> ('a * 'b) t
(** Stack progress bars vertically. [a <-> b] is a set with [a] stacked on top
    of [b]. The two bars have separate reporting functions (supplied as a pair). *)

val bytes : int64 pp_fixed
(** Fixed-width pretty-printer for counts in units of bytes. *)

(** {1 Rendering} *)

val with_display : ?ppf:Format.formatter -> 'a t -> ('a -> 'b) -> 'b
(** Render a set of progress bars inside a continuation.

    @param ppf Defaults to {!Format.err_formatter} *)

(** Functions for explicitly starting and stopping the process of rendering a
    bar; useful when the code doing the progress reporting cannot be
    conveniently delimited inside {!with_display}. All {!display}s must be
    properly {!finalise}d, and it is not possible to interleave rendering of
    displays. *)

type display

val start : ?ppf:Format.formatter -> 'a t -> 'a * display
val finalise : display -> unit

(** {1 Miscellaneous} *)

module Bytes = Bytes
(** Helpers for interpreting integer values as byte counts. *)
