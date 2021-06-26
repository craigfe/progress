(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

(** These values are documented as part of {!DSL}.*)
module type Integer_dependent = sig
  type integer
  type color
  type 'a printer
  type bar_style

  (**)
  type 'a t

  val count : ?pp:integer printer -> width:int -> unit -> integer t
  val count_up_to : ?pp:integer printer -> ?sep:unit t -> integer -> integer t
  val bytes : integer t
  val bytes_per_sec : integer t
  val percentage_of : integer -> integer t
  val rate : float printer -> integer t
  val eta : integer -> integer t

  type bar_style := [ `ASCII | `UTF8 | `Custom of bar_style ]

  val bar :
       ?style:bar_style
    -> ?color:color
    -> ?width:[ `Fixed of int | `Expand ]
    -> ?data:[ `Sum | `Latest ]
    -> integer
    -> integer t
end

(** The 'main' set of combinators, specialised to a particular integer type. *)
module type DSL = sig
  type integer
  type color
  type 'a printer

  type 'a t
  (** The type of progress lines for reported values of type ['a]. This module
      provides a selection of {{!basic} individual line segments} that can be
      {{!combinators} combined} to produce more interesting layouts. You may
      wish to look over the {{!examples} examples} for inspiration. *)

  (** {1:basic Basic line segments} *)

  val const : string -> _ t
  (** [const s] is the segment that always displays [s]. *)

  val constf : ('a, Format.formatter, unit, _ t) format4 -> 'a
  (** Like {!const}, but takes a format string and corresponding arguments.
      [constf "..." a b c ...] is equivalent to
      [const (Format.asprintf "..." a b c ...)], except that colours added with
      [Fmt.styled] are not discarded. *)

  val string : string t
  (** A line segment that displays a dynamically-sized string message. Use
      {!lpad} and {!rpad} to pad the message up to a given length. *)

  val lpad : int -> 'a t -> 'a t
  (** [lpad n t] left-pads the segment [t] to size [n] by adding blank space at
      the start. *)

  val rpad : int -> 'a t -> 'a t
  (** [rpad n t] right-pads the segment [t] to size [n] by adding blank space at
      the end. *)

  val of_printer : ?init:'a -> 'a printer -> 'a t
  (** [of_printer p] is a segment that renders the latest reported value using
      printer [p]. See {!accumulator} *)

  (** {2:counting Counting segments}

      These segments all consume integer values and display the accumulated
      total of all reported values in some way. The top-level [Line] segments
      are specialised to [int] values; see "{!integers}" for variants supporting
      [int32], [int64] etc. *)

  val count : ?pp:integer printer -> width:int -> unit -> integer t
  (** [count ~width ()] displays a running total of reported values using
      [width]-many terminal columns. If passed, [pp] overrides the printer used
      for rendering the count. *)

  val count_up_to : ?pp:integer printer -> ?sep:unit t -> integer -> integer t
  (** [count_up_to target] is like {!count}, but also renders the target total
      after a given separator, i.e. [42/100]. [sep] defaults to [const "/"]. The
      width of the segment is inferred by printing [total]. *)

  val bytes : integer t
  (** Prints the running total as a number of bytes, using ISO/IEC binary
      prefixes (e.g. [10.4 MiB]). See also {!bytes_per_sec}. *)

  val percentage_of : integer -> integer t
  (** [percentage_of target] renders the running total as a percentage of
      [target], i.e. [42%]. Values outside the range [\[0, 100\]] will be
      clamped to either [0] or [100]. *)

  (** {2:graphical Graphical segments} *)

  module Bar_style : sig
    type t
    (** The type of progress bar style specifications. *)

    val ascii : t
    (** The style used by [bar ~style:`ASCII] (which is the default). Generates
        bars of the form [\[######---\]]. *)

    val utf8 : t
    (** {!utf8} is the style used by [bar ~style:`UTF8]. Uses the UTF-8 block
        element characters ([U+2588]–[U+258F]) for progress stages, and a
        box-drawing character ([U+2502]) for delimiters. *)

    (** {1 Custom styles} *)

    val v :
         ?delims:string * string
      -> ?color:color
      -> ?color_empty:color
      -> string list
      -> t

    (** [v stages] is a bar that uses the given string {i stages} to render
        progress. The first stage is interpreted as a "full" segment, with
        subsequent stages denoting progressively {i less}-full segments until a
        final "empty" stage (which is implicitly a space if only one stage is
        provided).

        The optional parameters are as follows:

        - [?delims]: a pair of left and right delimiters used to wrap the body
          of the progress bar;
        - [?color]: the color of non-empty segments (including the in-progress
          one);
        - [?color_empty]: the color of empty segments.

        {2 Examples}

        - [v \[ "#" \]] renders like "[#######   ]";
        - [v \[ "="; ">"; " " \]] renders like "[======>    ]";
        - [v \[ "4"; "3"; "2"; "1"; "0" \]] renders like "[444444410000]";
        - ... see [examples/bar_styles.ml] in the source repository for more.

        {2 Specifics}

        Each segment of a rendering progress bar is in one of three states:
        full, empty or in-progress. At any given time, either the bar is
        entirely full or or there is exactly one in-progress segment. Given the
        style [v \[s1; s2; ... sN\]], these states are rendered as follows:

        - {b full}: rendered as [s1];
        - {b empty}: rendered as [sN] if [N >= 1], otherwise [' '];
        - {b in-progress}: if [N <= 1], then equivalent to the empty state.
          Otherwise, the intermediate stages [s2], [s3], ... [s{N-1}] denote
          decreasing progress. For example, if there are four intermediate
          stages ([N = 6]) then [s2] is used for progress in the range
          [\[0, 25%)], [s3] for [\[25%, 50%)] etc.

        For the progress bar to render within a fixed size, the user must ensure
        that each of the [stages] must have the same rendered width. *)

    (** {1 Setters} *)

    val with_color : color -> t -> t
    val with_empty_color : color -> t -> t
    val with_delims : (string * string) option -> t -> t
    val with_stages : string list -> t -> t
  end

  val bar :
       ?style:[ `ASCII | `UTF8 | `Custom of Bar_style.t ]
    -> ?color:color
    -> ?width:[ `Fixed of int | `Expand ]
    -> ?data:[ `Sum | `Latest ]
    -> integer
    -> integer t
  (** [bar total] is a progress bar of the form:
      [\[#################...............\]]

      The proportion of the bar that is filled is given by
      [<reported_so_far> / total]. Optional parameters are as follows:

      - [?style] specifies whether to use a UTF-8 or an ASCII encoding for the
        progress bar. The UTF-8 encoding shows a higher resolution of progress,
        but may not be supported in all terminals. The default is [`ASCII].

      - [?width] is the width of the bar in columns. Defaults to [`Expand],
        which causes the bar to occupy the remaining rendering space after
        accounting for other line segments on the same line. *)

  val spinner :
       ?color:color
    -> ?frames:string list
    -> ?min_interval:Duration.t option
    -> unit
    -> _ t

  val ticker : unit -> _ t
  (** TODO: document. *)

  val ticker_up_to : ?sep:unit t -> integer -> _ t
  (** TODO: document. *)

  (** {2:time Time-sensitive segments} *)

  val bytes_per_sec : integer t
  val rate : float printer -> integer t

  val elapsed : ?pp:Duration.t printer -> unit -> _ t
  (** Displays the time for which the bar has been rendering in [MM:SS] form. *)

  val eta : integer -> integer t
  (** Displays an estimate of the remaining time until [total] is accumulated by
      the reporters, in [MM:SS] form. *)

  (** {1:combinators Combining segments} *)

  val ( ++ ) : 'a t -> 'a t -> 'a t
  (** Horizontally join two segments of the same reported value type. *)

  val list : ?sep:'a t -> 'a t list -> 'a t
  (** Horizontally join a list of segments, with a given separator. [sep]
      defaults to [const " "]. *)

  val pair : ?sep:unit t -> 'a t -> 'b t -> ('a * 'b) t
  (** Horizontally join a pair of segments consuming different reported values
      into a single segment that consumes a pair. *)

  val using : ('a -> 'b) -> 'b t -> 'a t
  (** [using f s] is a segment that first applies [f] to the reported value and
      then behaves as segment [s]. *)

  (** {1 Utilities}

      The following line segments are definable in terms of the others, but
      provided for convenience: *)

  val parens : 'a t -> 'a t
  (** [parens t] is [const "(" ++ t ++ const ")"]. *)

  val brackets : 'a t -> 'a t
  (** [brackets t] is [const "\[" ++ t ++ const "\]"]. *)

  val braces : 'a t -> 'a t
  (** [braces t] is [const "{" ++ t ++ const "}"]. *)

  val noop : unit -> _ t
  (** A zero-width line segment that does nothing. This segment will not be
      surrounded with separators when used in a {!list}, making it a useful
      "off" state for conditionally-enabled segments. *)
end

module Assert_subtype (X : DSL) :
  Integer_dependent with type bar_style := X.Bar_style.t =
  X

module type S = sig
  include DSL with type integer := int
  (** @inline *)

  (** {1:integers Alternative integer types}

      Many of the line segments above are specialised to [int] values for
      simplicity (and performance), but certain use-cases may require different
      types (e.g. for file transfers greater than [2 GiB] on 32-bit platforms).
      The following modules re-export the [Line] DSL with different integer
      speciialisations, and are intended to be opened locally, e.g.

      {[
        let my_line =
          let open Progress.Line.Using_int64 in
          list [ const "Downloading large file"; bar total; bytes ]
      ]} *)

  module Integer_dependent : sig
    (** {!S} contains just the line segments that can be specialised to an
        underlying integer implementation. *)
    module type S =
      Integer_dependent
        with type 'a t := 'a t
         and type color := color
         and type 'a printer := 'a printer
         and type bar_style := Bar_style.t

    module Make (Integer : Integer.S) : S with type integer := Integer.t

    (** {!Ext} is {!S} extended with non-integer-dependent segments as well. *)
    module type Ext =
      DSL
        with type 'a t := 'a t
         and type color := color
         and type 'a printer := 'a printer
         and type Bar_style.t := Bar_style.t
  end

  module Using_int32 : Integer_dependent.Ext with type integer := int32
  module Using_int63 : Integer_dependent.Ext with type integer := int63
  module Using_int64 : Integer_dependent.Ext with type integer := int64
  module Using_float : Integer_dependent.Ext with type integer := float

  (** {1:examples Examples}

      - A

      {[
        [##################################################################] 100/100
      ]}
      {[ (* Described by: *) const " " ++ count ~up_to:100 ]}
      - An progress bar for a file download:

      {[
        ⠏ [01:04] [######---------------------------------------]  293.9 MiB (eta: 07:12)
      ]}
      {[
        (* Described by: *)
        list
          [ spinner ()
          ; brackets (elapsed ())
          ; bar total
          ; bytes
          ; parens (const "eta: " ++ eta total)
          ]
      ]}

      See the [examples/] directory of the source repository for more. *)

  (** {1 Library internals} *)

  module Internals : sig
    (** Exposes the underlying implementation of line segments for testing. This
        API is unstable, unsafe and mostly undocumented; here be dragons etc. *)

    type 'a line

    module Line_buffer = Line_buffer

    include Line_primitives.S with type 'a t = 'a Line_primitives.t
    (** @inline *)

    val box_winsize : ?max:int -> ?fallback:int -> 'a t -> 'a t
    (** A box that takes on the current size of the terminal (or [fallback] if
        stdout is not attached to a terminal).

        @param fallback defaults to [80].
        @param max defaults to no limit. *)

    val to_line : 'a t -> 'a line
  end
  with type 'a line := 'a t
end

module type Line = sig
  module type S = S

  type 'a t

  module Make (_ : Platform.S) : sig
    include
      S
        with type 'a t := 'a t
         and type color := Terminal.Color.t
         and type 'a printer := 'a Printer.t

    val to_primitive : Config.t -> 'a t -> 'a Internals.t
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
