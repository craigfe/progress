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
      {!Progress_logs} instead.

      {b Note}: the caller must ensure that the terminal cursor is left in an
      appropriate position to resume rendering. In practice, this means that any
      printing to the terminal should be terminated with a newline character and
      flushed. *)

  (** {2 Manual lifecycle management}

      Functions for explicitly starting and stopping the process of rendering a
      bar; useful when the code doing the progress reporting cannot be
      conveniently delimited inside {!with_reporter}. All {!Display}s must be
      properly {{!Display.finalise} finalised}, and it is not possible to
      interleave rendering of displays. *)

  module Reporter : sig
    type 'a t

    val push : 'a t -> 'a -> unit
    val noop : 'a t

    type (_, _) list =
      | [] : ('a, 'a) list
      | ( :: ) : 'a * ('b, 'c) list -> ('a -> 'b, 'c) list
  end

  module Display : sig
    type ('a, 'b) t
    (** The type of active progress bar displays. The type parameters ['a] and
        ['b] store a list of the reporting functions used *)

    val start : ?config:config -> ('a, 'b) multi -> ('a, 'b) t
    (** Initiate rendering of a progress bar display. Raises [Failure] if there
        is already an active progress bar display. *)

    val reporters : ('a, unit) t -> ('a, unit) Reporter.list
    (** [reporters d] is the list of reporting functions for the .

        {b Note:} this list does not include any reporters added {i during}
        progress bar rendering via {!add_line}. *)

    val tick : _ t -> unit

    val add_line : ?above:int -> (_, _) t -> 'a line -> 'a Reporter.t
    (** Add a line to an ongoing display, and get its reporting function. *)

    val finalise_line : (_, _) t -> _ Reporter.t -> unit

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
