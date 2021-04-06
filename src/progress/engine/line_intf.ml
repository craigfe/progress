module type Integer_dependent = sig
  type 'a t
  type integer

  val of_printer : integer Printer.t -> integer t
  (** [of_pp ~width pp] is a segment that uses the supplied fixed-width
      pretty-printer to render the value. The pretty-printer must never emit
      newline characters. *)

  val count : integer -> integer t
  (** [counter pp] is a segment that uses the supplied fixed-width
      pretty-printer to print the {i accumulated} total of all values.

      The pretty-printer must never emit newline characters. *)

  val bytes : integer t
  val percentage_of : integer -> integer t
  val max : integer -> integer t
  val rate : float Printer.t -> integer t
  val eta : total:integer -> integer t

  val bar :
       ?style:[ `ASCII | `UTF8 | `Custom of string list ]
    -> ?color:Ansi.Color.t
    -> ?color_empty:Ansi.Color.t
    -> ?width:[ `Fixed of int | `Expand ]
    -> total:integer
    -> unit
    -> integer t
  (** [bar ~width f] is a progress bar of the form:

      {[ [#######################################................] ]}

      which occupies [width]-many columns and uses [f] to determine the
      proportion of the bar that is filled.

      If [~width:`Expand] is passed – which is the default – this segment
      must be contained inside a {{!boxes} box} that determines its size. *)
end

module type S = sig
  type 'a t
  (** The type of segments of progress bars that display reported values of type
      ['a]. *)

  (** {2 Pre-provided segments} *)

  val const : string -> _ t
  (** [const s] is the segment that always displays [s]. [s] must not contain
      any newline characters. *)

  val constf : ('a, Format.formatter, unit, _ t) format4 -> 'a
  (** Like {!const}, but takes a format string and placeholder values rather
      than a string. i.e. [constf fmt a b c ...] is equivalent to
      [const (Format.asprintf fmt a b c ...)]. *)

  val string : string t

  val elapsed : unit -> 'a t
  (** Displays the time for which the bar has been rendering in [MM:SS] form. *)

  val lpad : int -> 'a t -> 'a t
  val rpad : int -> 'a t -> 'a t
  val spinner : ?color:Ansi.Color.t -> ?stages:string list -> unit -> _ t

  (** @inline *)
  include Integer_dependent with type 'a t := 'a t and type integer := int

  module Integer_dependent (Integer : Integer.S) :
    Integer_dependent with type 'a t := 'a t and type integer := Integer.t

  (** {2 Combining segments} *)

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

  val noop : unit -> _ t
  (** A line segment that does nothing. *)

  module Expert : sig
    include Segment.S with type 'a t = 'a Segment.t
    (** @inline *)

    val box_winsize : ?max:int -> ?fallback:int -> 'a t -> 'a t
    (** A box that takes on the current size of the terminal (or [fallback] if
        stdout is not attached to a terminal).

        @param fallback defaults to [80].
        @param max defaults to no limit. *)
  end
end

module Types = struct
  type render_config = { interval : Mtime.span option; max_width : int option }
end

module type Line = sig
  module type S = S

  include module type of Types

  type 'a t

  module Platform_dependent (_ : Platform.S) : sig
    include S with type 'a t := 'a t

    val compile : 'a t -> config:render_config -> 'a Expert.t
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
