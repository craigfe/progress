(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

module type S = sig
  (* We go to some effort here to avoid having types directly refer to those in
     [Progress_engine] rather than going via the aliases. *)

  (** *)

  (** {1 Preliminaries}

      Some basic types used throughout the rest of the library: *)

  module Color = Terminal_ansi.Color
  module Duration = Duration
  module Printer = Printer
  module Units = Units

  (** {1 Description}

      Describing a progress line is done via the {!Line} DSL. Individual lines
      can be stacked vertically via {!Multi}. *)

  module Line : sig
    (** @inline *)
    include
      Line.S
        with type 'a t = 'a Line.t
         and type color := Color.t
         and type duration := Duration.t
         and type 'a printer := 'a Printer.t
  end

  module Multi : sig
    type 'a reporter := 'a -> unit

    (** @inline *)
    include
      Multi.S with type 'a line := 'a Line.t and type 'a reporter := 'a reporter
  end

  (** {2 Pre-provided lines} *)

  val counter :
       ?style:[ `ASCII | `UTF8 | `Custom of Line.Bar_style.t ]
    -> ?message:string
    -> ?pp:int64 Printer.t
    -> int64
    -> int64 Line.t
  (** [counter total] is a progress bar of the form:

      {[
        <message?> <count?> MM:SS [########..............................]  XX%
      ]}

      where each reported value contributes cumulatively towards an eventual
      total of [total]. [?style] specifies the
      {{!Line.Bar_style.t} [Bar_style.t]} to use for rendering the bar, and
      [?pp] is used to pretty-print the [<count>] segment, if passed. (For
      example, {!Units.Bytes.of_int64} can be used for totals measured in
      bytes.) *)

  (** {1 Rendering}

      Once you have a {{!description} description} of the progress bar to be
      rendered (either a {!Line.t} or a {!Multi.t}), begin rendering it by using
      {!with_reporter} or {!with_reporters} respectively. *)

  type 'a reporter := 'a -> unit
  (** A {i reporter} for values of type ['a]. In this library, each progress
      line has its own reporting function. *)

  (** Configuration for progress bar rendering. *)
  module Config : sig
    type t

    val v :
         ?ppf:Format.formatter
      -> ?hide_cursor:bool
      -> ?persistent:bool
      -> ?max_width:int option
      -> ?min_interval:Duration.t option
      -> unit
      -> t
    (** - [ppf]: the formatter to use for rendering. Defaults to
          [Format.err_formatter].

        - [hide_cursor]: whether or not to hide the terminal cursor (using the
          {{:https://en.wikipedia.org/wiki/ANSI_escape_code} [DECTCEM]} ANSI
          escape codes) during progress bar rendering. Defaults to [true].

        - [persistent]: whether or not to retain the final progress bar display
          in the terminal after rendering has finished. Defaults to [true].

        - [max_width]: an optional fixed upper bound on the size of a progress
          bar (in addition to the one by the terminal width). Defaults to
          [None].

        - [min_interval]: the minimum time interval between re-renders of the
          progress bar display (i.e. a debounce threshold). Defaults to 1/60th
          of a second. *)

    val ( || ) : t -> t -> t
    (** Merge two config values, with settings from the left taking priority.
        That is, [a || b] contains the configuration of [a], with unset defaults
        taken from [b]. *)

    (** Provides the default values of each of the config parameters. *)
    module Default : sig
      val ppf : Format.formatter
      (** [ppf] is [Format.err_formatter]. *)

      val hide_cursor : bool
      (** [hide_cursor] is [true]. *)

      val persistent : bool
      (** [persistent] is [true]. *)

      val max_width : int option
      (** [max_width] is [None]. *)

      val min_interval : Duration.t option
      (** [min_interval] is 1/60th of a second. *)
    end
  end

  (** @inline *)
  include
    Renderer.S
      with type 'a reporter := 'a reporter
       and type 'a line := 'a Line.t
       and type ('a, 'b) multi := ('a, 'b) Multi.t
       and type config := Config.t
end

module type Progress_engine = sig
  module type S = S
  module type Platform = Platform.S

  module Make (_ : Platform) : S
  module Integer = Integer

  module Exposed_for_testing : sig
    module Flow_meter = Flow_meter
  end
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
