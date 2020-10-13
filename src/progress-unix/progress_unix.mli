(** This module defines Unix-specific extensions to the {!Progress} library.

    It is packaged as the Dune library [progress.unix]. *)

(** {2 Time-sensitive segments} *)

val stopwatch : unit -> 'a Progress.Segment.t
(** Displays the time for which the bar has been rendering in [MM:SS] form. *)

val counter :
  total:int64 ->
  ?mode:[ `ASCII | `UTF8 ] ->
  ?message:string ->
  ?pp:(int64, int64 Progress.Segment.t) Progress.Units.pp_fixed ->
  ?width:int ->
  ?sampling_interval:int ->
  unit ->
  (int64 -> unit) Progress.t
(** [counter ~total ()] is a progress bar of the form:

    {[
      <message?>  <count?>  MM:SS  [########..............................]  XX%
    ]}

    This is an equivalent of {!Progress.counter} with an extra {!stopwatch}
    segment. *)

(** {2 TTY-sensitive renderers} *)

val stderr_if_tty : Format.formatter
(** A formatter that is equal to {!Format.err_formatter} if stdout is a TTY, and
    is a noop formatter otherwise. *)

(** Renderers that use {!stderr_if_tty} as an output formatter. *)

val with_reporters : 'a Progress.t -> ('a -> 'b) -> 'b
val start : 'a Progress.t -> 'a * Progress.display
val finalise : Progress.display -> unit

(** {2 Re-exports}

    Convenient aliases to functions defined in {!Progress}. *)

val ( / ) : 'a Progress.t -> 'b Progress.t -> ('a * 'b) Progress.t
