(** A library for displaying progress bars, including support for rendering
    multiple bars at once. Start by {{!description} describing} of a set of
    progress bars, then begin {{!rendering} rendering} them to get access to
    their respective reporting functions.

    See {!Progress_unix} for access to Unix-specific utilities. *)

(** {1 Description} *)

type 'a t
(** The type of progress bars with reporting functions of type ['a]. You'll get
    access to the reporting functions when beginning the {{!rendering}
    rendering} process. *)

val counter :
  total:int64 ->
  ?mode:[ `ASCII | `UTF8 ] ->
  ?message:string ->
  ?pp:(int64, int64 Segment.t) Units.pp_fixed ->
  ?width:int ->
  ?sampling_interval:int ->
  unit ->
  (int64 -> unit) t
(** [counter ~total ()] is a progress bar of the form:

    {[ <message?>  <count?>  [########..............................]  XX% ]}

    where each reported value contributes cumulatively towards an eventual total
    of [total]. Optional parameters are as follows:

    - [?mode] specifies whether to use a UTF-8 or an ASCII encoding for the
      progress bar. The UTF-8 encoding shows a higher resolution of progress,
      but may not be supported in all terminals. The default is [`ASCII].

    - [?pp] is used to pretty-print the [<count>] segment, if passed. For
      example, {!Units.bytes} can be used for totals measured in bytes. The
      default is to not display this segment.

    - [?width] is the width of the bar in columns. Defaults to the width of
      [stdout], if it is a terminal.

    - [?sampling_interval] specifies the number of reported values that must be
      passed for each update of the display (not including the update during
      finalisation, which always occurs). This is useful when progress is being
      reported from a hot-loop, where the cost of re-displaying the progress bar
      is non-negligible. The default value is [1], meaning that all updates are
      displayed immediately.

    See {!Progress_unix.counter} for an equivalent that contains a timer. *)

(** [Segment] contains a DSL for defining custom progress bars. *)
module Segment : sig
  include Segment.S
  (** @inline *)
end

val make : init:'a -> 'a Segment.t -> ('a -> unit) t
(** Define a new progress bar from a specification, with the given initial
    value. *)

(** {2 Multiple progress bars} *)

val ( / ) : 'a t -> 'b t -> ('a * 'b) t
(** Stack progress bars vertically. [a / b] is a set with [a] stacked on top of
    [b]. The two bars have separate reporting functions (supplied as a pair). *)

(** {1 Rendering} *)

val with_reporters : ?ppf:Format.formatter -> 'a t -> ('a -> 'b) -> 'b
(** Render a set of progress bars inside a continuation.

    @param ppf Defaults to {!Format.err_formatter} if [stderr] is a TTY, and is
    a noop formatter otherwise. *)

(** Functions for explicitly starting and stopping the process of rendering a
    bar; useful when the code doing the progress reporting cannot be
    conveniently delimited inside {!with_display}. All {!display}s must be
    properly {!finalise}d, and it is not possible to interleave rendering of
    displays. *)

type display

val start : ?ppf:Format.formatter -> 'a t -> 'a * display
val finalise : display -> unit

(** {1 Miscellaneous} *)

module Units = Units
(** Helpers for printing values of various units. *)

(** Internals of the library exported to be used in sibling packages and in
    testing. Not intended for public consumption, and does not provide a stable
    API. *)
module Internal : sig
  val counter :
    ?prebar:int64 Segment.t ->
    total:int64 ->
    ?mode:[ `ASCII | `UTF8 ] ->
    ?message:string ->
    ?pp:(int64, int64 Segment.t) Units.pp_fixed ->
    ?width:int ->
    ?sampling_interval:int ->
    unit ->
    (int64 -> unit) t
end
