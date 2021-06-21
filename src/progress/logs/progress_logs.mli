(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(** Extensions to the {{:https://erratique.ch/software/logs} [Logs]} library
    designed to cooperate with progress bar rendering. *)

val reporter :
     ?pp_header:(Logs.level * string option) Fmt.t
  -> ?app:Format.formatter
  -> ?dst:Format.formatter
  -> unit
  -> Logs.reporter
(** [reporter] is like [Logs_fmt.reporter] but produces a reporter that
    {{!Progress.interject_with} suspends} any ongoing progress bar rendering
    while displaying log entries, ensuring that log entries in the terminal are
    never overwritten by the renderer. *)

val instrument_reporter : Logs.reporter -> Logs.reporter
(** [instrument_reporter r] wraps the synchronous reporter [r] to ensure that
    any progress bar rendering is suspended while messages are being constructed
    for [r].

    {b Note}: to ensure that log entries are not overwritten by the [Progress]
    renderer, [r] must flush any log entries to the terminal {i synchronously}:
    as soon as they are reported. This is true of the [Logs] reporters built by
    {!Logs.format_reporter} and {!Logs_fmt.reporter}. An asynchronous reporter
    should use {!Progress.interject_with} to delimit its flushing action
    instead. *)

(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
  ————————————————————————————————————————————————————————————————————————————*)
