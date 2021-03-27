(** A library for displaying progress bars, including support for rendering
    multiple bars at once. Start by {{!description} describing} of a sequence of
    progress bars, then begin {{!rendering} rendering} them to get access to
    their respective reporting functions.

    - See {!Progress_unix} for access to Unix-specific utilities.
    - See {!Progress_logs} for related extensions to the
      {{:https://erratique.ch/software/logs} Logs} library. *)

(** {1 Description} *)

type 'a reporter = 'a -> unit
(** A {i reporter} for values of type ['a]. In this library, each progress bar
    has its own reporting function. *)

type ('a, 'b) t
(** The type of sequences of progress bars. The parameter ['a] stores a list of
    the reporting functions associated with each bar, terminating with ['b]. For
    example:

    {[
      (* Single progress bar, taking a [float] value. *)
      (float reporter -> 'b, 'b) t

      (* A two-bar layout, where the top bar takes [int64]s and the bottom one
         takes [string * float] pairs. *)
      (int64 reporter -> (string * float) reporter -> 'b, 'b) t
    ]}

    These reporting functions are supplied when beginning the {{!rendering}
    rendering} process. *)

val counter :
  total:int64 ->
  ?mode:[ `ASCII | `UTF8 ] ->
  ?message:string ->
  ?pp:(int64, int64 Segment.t) Units.pp_fixed ->
  ?width:int ->
  ?sampling_interval:int ->
  unit ->
  (int64 reporter -> 'a, 'a) t
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

val make : init:'a -> 'a Segment.t -> ('a reporter -> 'b, 'b) t
(** Define a new progress bar from a specification, with the given initial
    value. *)

(** {2 Multiple progress bars} *)

val ( / ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
(** Stack progress bars vertically. [a / b] is a set with [a] stacked on top of
    [b]. The two bars have separate reporting functions, passed consecutively to
    the {!with_reporters} continuation when rendering. *)

(** A list of reporters of differing types. *)
module Reporters : sig
  type 'a t = [] : unit t | ( :: ) : 'a * 'b t -> ('a -> 'b) t
end

val list : ('a, unit) t list -> ('a Reporters.t list -> 'b, 'b) t
(** TODO *)

(** {1 Rendering} *)

(** Configuration for progress bar rendering. *)
module Config : sig
  type t

  val create :
    ?ppf:Format.formatter -> ?hide_cursor:bool -> ?persistent:bool -> unit -> t
  (** @param ppf The formatter to use for rendering. Defaults to
      [Format.err_formatter].
      @param hide_cursor Whether or not to hide the terminal cursor (using the
      {{:https://en.wikipedia.org/wiki/ANSI_escape_code} [DECTCEM]} ANSI escape
      codes) during progress bar rendering. Defaults to [true]. *)

  val ( || ) : t -> t -> t
end

val with_reporters : ?config:Config.t -> ('a, 'b) t -> 'a -> 'b
(** [with_reporters bars f] renders [bars] inside the continuation [f], after
    supplying [f] with the necessary reporting functions. For example:

    {[
      (** Reading a file into memory with a single progress bar. *)
      let read_file path buffer =
        let total = file_size path and in_channel = open_in path in
        try
          with_reporters (counter ~total ()) @@ fun report ->
          let rec aux offset =
            let bytes_read = really_read buffer offset in
            report bytes_read;
            aux (offset + bytes_read)
          in
          aux 0
        with End_of_file -> close_in in_channel

      (** Sending data to multiple clients, with one progress bar each. *)
      let multi_bar_rendering () =
        with_reporters
          (bar_a / bar_b / bar_c)
          (fun report_a report_b report_c ->
            for i = 1 to 1000 do
              report_a (transfer_bytes client_a);
              report_b (transfer_bytes client_b);
              report_c (transfer_bytes client_c)
            done)
    ]} *)

val interject_with : (unit -> 'a) -> 'a
(** [interject_with f] executes the function [f] while temporarily suspending
    the rendering of any active progress bar display. This can be useful when
    printing to [stdout] / [stderr], to avoid any interference from the
    rendering of progress bars. If using the [Logs] library, consider using
    {!Progress_logs} instead.

    {b Note}: the caller must ensure that the terminal cursor is left in an
    appropriate position to resume rendering. In practice, this means that any
    printing to the terminal should be terminated with a newline character. *)

type display
(** Functions for explicitly starting and stopping the process of rendering a
    bar; useful when the code doing the progress reporting cannot be
    conveniently delimited inside {!with_display}. All {!display}s must be
    properly {!finalize}d, and it is not possible to interleave rendering of
    displays. *)

val start : ?config:Config.t -> ('a, unit) t -> 'a Reporters.t * display
(** Initiate rendering of a progress bar display.

    @raise Failure if there is already an active progress bar display. *)

val finalize : display -> unit
(** Terminate the given progress bar display.

    @raise Failure if the display has already been finalized. *)

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
    (int64 reporter -> 'a, 'a) t
end
