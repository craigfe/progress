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

  val with_reporters : ?config:config -> ('a, 'b) multi -> 'a -> 'b
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

  module Reporter : sig
    type 'a t

    val push : 'a t -> 'a -> unit
    val noop : 'a t
  end

  module Reporters : sig
    type (_, _) t =
      | [] : ('a, 'a) t
      | ( :: ) : 'a * ('b, 'c) t -> ('a -> 'b, 'c) t
  end

  (** Functions for explicitly starting and stopping the process of rendering a
      bar; useful when the code doing the progress reporting cannot be
      conveniently delimited inside {!with_display}. All {!display}s must be
      properly {!finalize}d, and it is not possible to interleave rendering of
      displays. *)
  module Display : sig
    type ('a, 'b) t

    val start : ?config:config -> ('a, 'b) multi -> ('a, 'b) t
    (** Initiate rendering of a progress bar display.

        @raise Failure if there is already an active progress bar display. *)

    val tick : _ t -> unit
    val reporters : ('a, 'b) t -> ('a, 'b) Reporters.t
    val add_line : ?above:int -> (_, _) t -> 'a line -> 'a Reporter.t
    val finalize_line : (_, _) t -> _ Reporter.t -> unit

    val finalize : (_, _) t -> unit
    (** Terminate the given progress bar display.

        @raise Failure if the display has already been finalized. *)
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
