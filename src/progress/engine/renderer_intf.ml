(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

type 'a reporter = 'a -> unit

module type S = sig
  type 'a reporter
  type 'a line
  type ('a, 'b) multi
  type config

  val with_reporter : ?config:config -> 'a line -> ('a reporter -> 'b) -> 'b
  (** [with_reporters line f] begins rendering [line] and calls [f] with the
      reporting function. Once [f] returns, the display is finalised. {b Note:}
      attempting to use the reporting function after [f] has returned will raise
      a [Finalised] exception. *)

  val with_reporters : ?config:config -> ('a, 'b) multi -> 'a -> 'b
  (** [with_reporters bars f] begins rendering [bars] and passes the
      corresponding reporting functions to [f]. Once [f] returns, the display is
      finalised. *)

  (** {2 Examples}

      - Reading a file into memory and displaying a single progress bar:

      {[
        let read_file path buffer =
          let total = file_size path and in_channel = open_in path in
          try
            with_reporter (counter ~total ()) @@ fun report ->
            let rec aux offset =
              let bytes_read = really_read buffer offset in
              report bytes_read;
              aux (offset + bytes_read)
            in
            aux 0
          with End_of_file -> close_in in_channel
      ]}
      - Sending data to multiple clients, with one progress bar each:

      {[
        let multi_bar_rendering () =
          with_reporters
            Multi.(line bar_a ++ line bar_b ++ line bar_c)
            (fun report_a report_b report_c ->
              for i = 1 to 1000 do
                report_a (transfer_bytes client_a);
                report_b (transfer_bytes client_b);
                report_c (transfer_bytes client_c)
              done)
      ]} *)

  (** {2 Logging during rendering} *)

  val interject_with : (unit -> 'a) -> 'a
  (** [interject_with f] executes the function [f] while temporarily suspending
      the rendering of any active progress bar display. This can be useful when
      printing to [stdout] / [stderr], to avoid any interference from the
      rendering of progress bars. If using the [Logs] library, consider using
      {!reporter} and {!instrument_reporter} instead.

      {b Note}:
      {i the caller must ensure that the terminal cursor is left in an
         appropriate position to resume rendering. In practice, this means that
         any printing to the terminal should be terminated with a newline
         character and flushed.} *)

  (** Extensions to the {{:https://erratique.ch/software/logs} [Logs]} library
      designed to cooperate with progress bar rendering: *)

  val logs_reporter :
       ?pp_header:(Logs.level * string option) Fmt.t
    -> ?app:Format.formatter
    -> ?dst:Format.formatter
    -> unit
    -> Logs.reporter
  (** [reporter] is like [Logs_fmt.reporter] but produces a reporter that
      {{!Progress.interject_with} suspends} any ongoing progress bar rendering
      while displaying log entries, ensuring that log entries in the terminal
      are never overwritten by the renderer. *)

  val instrument_logs_reporter : Logs.reporter -> Logs.reporter
  (** [instrument_reporter r] wraps the synchronous reporter [r] to ensure that
      any progress bar rendering is suspended while messages are being
      constructed for [r].

      {b Note}:
      {i to ensure that log entries are not overwritten by the [Progress]
         renderer, [r] must flush any log entries to the terminal synchronously:
         as soon as they are reported. This is true of the [Logs] reporters
         built by {!Logs.format_reporter} and {!Logs_fmt.reporter}. An
         asynchronous reporter should use {!interject_with} to delimit its
         flushing action instead.} *)

  (** {2 Manual lifecycle management}

      Functions for explicitly starting and stopping the process of rendering a
      bar; useful when the code doing the progress reporting cannot be
      conveniently delimited inside {!with_reporter}. All {!Display}s must be
      properly {{!Display.finalise} finalised}, and it is not possible to
      interleave rendering of displays. *)

  module Reporter : sig
    type -'a t
    (** The (abstract) type of reporter functions used by the manual lifecycle
        management functions in {!Display}. An ['a t] is conceptually an
        ['a -> unit] function, but can be explicitly {!finalise}d. *)

    val report : 'a t -> 'a -> unit

    val finalise : _ t -> unit
    (** [finalise t] terminates rendering of the line associated with reporter
        [t]. Attempting to {!report} to a finalised reporter will raise an
        exception. *)

    (** A heterogeneous list type, used by {!Display} for returning a list of
        reporters corresponding to multi-line progress displays. *)
    type (_, _) list =
      | [] : ('a, 'a) list
      | ( :: ) : 'a * ('b, 'c) list -> ('a -> 'b, 'c) list
  end

  module Display : sig
    type ('a, 'b) t
    (** The type of active progress bar displays. The type parameters ['a] and
        ['b] track the types of the reporting functions supplied by {!reporters}
        (see {!Multi.t} for details).*)

    val start : ?config:config -> ('a, 'b) multi -> ('a, 'b) t
    (** Initiate rendering of a progress bar display. Raises [Failure] if there
        is already an active progress bar display. *)

    val reporters : ('a, unit) t -> ('a, unit) Reporter.list
    (** [reporters d] is the list of initial reporting functions belonging to
        display [d].

        {b Note}
        {i this list does not include any reporters added {i during} progress
           bar rendering via {!add_line}.} *)

    val tick : _ t -> unit
    (** [tick d] re-renders the contents of display [d] without reporting any
        specific values. This function can be used to update spinners,
        durations, etc. when there is no actual progress to report. *)

    val add_line : ?above:int -> (_, _) t -> 'a line -> 'a Reporter.t
    (** Add a line to an ongoing display, and get its reporting function. By
        default, the line is added to the {i bottom} of the display
        ([above = 0]); the [~above] argument can be passed to add the line above
        some number of existing lines. *)

    val remove_line : (_, _) t -> _ Reporter.t -> unit
    (** Remove a line to an ongoing display, from its reporting function.
        Removing an existing line is idempotent, but removing a line that was
        not at point point part of the display will raise an error. *)

    val finalise : (_, _) t -> unit
    (** Terminate the given progress bar display. Raises [Failure] if the
        display has already been finalised. *)
  end
end

module type Renderer = sig
  module type S = S

  module Make (_ : Platform.S) :
    S
      with type 'a reporter := 'a reporter
       and type 'a line := 'a Line.t
       and type ('a, 'b) multi := ('a, 'b) Multi.t
       and type config := Config.user_supplied
end

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
