(** Extensions to the {{:https://erratique.ch/software/logs} [Logs]} library
    designed to cooperate with progress bar rendering. *)

val reporter :
     ?pp_header:(Logs.level * string option) Fmt.t
  -> ?app:Format.formatter
  -> ?dst:Format.formatter
  -> unit
  -> Logs.reporter
(** [reporter] is like {!Logs_fmt.reporter} but produces a reporter that
    {{!Progress.interject_with} suspends} any ongoing progress bar rendering
    while displaying log entries, ensuring that log entries in the terminal are
    never overwritten by the renderer. *)

val instrument_reporter : Logs.reporter -> Logs.reporter
(** [instrument_reporter r] wraps reporter [r] to ensure that any progress bar
    rendering is suspended while messages are being constructed for [r].

    {b Note}: to ensure that log entries are not overwritten by the [Progress]
    renderer, [r] must flush any prints to the terminal {i synchronously}. An
    asynchronous reporter should use {!Progress.interject_with} to delimit its
    flushing action instead. *)
