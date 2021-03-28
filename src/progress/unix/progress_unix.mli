(** This module defines Unix-specific extensions to the {!Progress} library.

    It is packaged as the Dune library [progress.unix]. *)

(** {2 Time-sensitive segments} *)

val elapsed : unit -> 'a Progress.Segment.t
(** Displays the time for which the bar has been rendering in [MM:SS] form. *)

type 'a pp := Format.formatter -> 'a -> unit

val rate : int64 pp * int -> int64 Progress.Segment.t
val eta : int64 -> int64 Progress.Segment.t

type 'a accumulated

val acc : 'a accumulated -> 'a
val latest : 'a accumulated -> 'a

val debounced_accumulator :
     Progress.Duration.t
  -> ('a -> 'a -> 'a)
  -> 'a
  -> 'a accumulated Progress.Segment.t
  -> 'a Progress.Segment.t
(** [debounce span s] has the same output format as [s], but only passes
    reported values doen to [s] at most once in any given time [span]. *)

val counter :
     total:'elt
  -> ?color:Fmt.style
  -> ?style:Progress.bar_style
  -> ?message:string
  -> ?pp:'elt pp * int
  -> ?width:int
  -> ?sampling_interval:int
  -> (module Progress.Elt with type t = 'elt)
  -> ('elt Progress.reporter -> 'a, 'a) Progress.t
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

val with_reporters :
  ?config:Progress.Config.t -> ('a, 'b) Progress.t -> 'a -> 'b

val start :
     ?config:Progress.Config.t
  -> ('a, unit) Progress.t
  -> 'a Progress.Reporters.t * Progress.display

val finalize : Progress.display -> unit

(** {2 Re-exports}

    Convenient aliases to functions defined in {!Progress}. *)

val ( / ) : ('a, 'b) Progress.t -> ('b, 'c) Progress.t -> ('a, 'c) Progress.t
