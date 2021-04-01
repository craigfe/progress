(** This module defines Unix-specific extensions to the {!Progress} library.

    It is packaged as the Dune library [progress.unix]. *)

open Progress

type 'a pp := Format.formatter -> 'a -> unit

val counter :
     total:'elt
  -> ?color:Ansi.style
  -> ?style:bar_style
  -> ?message:string
  -> ?pp:'elt pp * int
  -> ?width:int
  -> ?sampling_interval:int
  -> (module Elt with type t = 'elt)
  -> ('elt reporter -> 'a, 'a) t
(** [counter ~total ()] is a progress bar of the form:

    {[
      <message?>  <count?>  MM:SS  [########..............................]  XX%
    ]}

    This is an equivalent of {!Progress.counter} with an extra {!stopwatch}
    line. *)

(** {2 Time-sensitive line segments} *)

include Line.Time_sensitive

(** {2 TTY-sensitive renderers} *)

val stderr_if_tty : Format.formatter
(** A formatter that is equal to {!Format.err_formatter} if stdout is a TTY, and
    is a noop formatter otherwise. *)

(** Renderers that use {!stderr_if_tty} as an output formatter. *)

val with_reporters : ?config:Config.t -> ('a, 'b) t -> 'a -> 'b
val start : ?config:Config.t -> ('a, unit) t -> 'a Reporters.t * display
val finalize : display -> unit

(** {2 Re-exports}

    Convenient aliases to functions defined in {!Progress}. *)

val ( / ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
