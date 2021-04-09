module type Integer_dependent = sig
  type 'a t
  type integer
  type color
  type 'a printer

  val of_printer : integer printer -> integer t
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
  val rate : float printer -> integer t
  val eta : total:integer -> integer t

  val bar :
       ?style:[ `ASCII | `UTF8 | `Custom of string list ]
    -> ?color:color
    -> ?color_empty:color
    -> ?width:[ `Fixed of int | `Expand ]
    -> total:integer
    -> unit
    -> integer t
  (** [bar ~width f] is a progress bar of the form:

      {[ [#######################################................] ]}

      which occupies [width]-many columns and uses [f] to determine the
      proportion of the bar that is filled. Optional parameters are as follows:

      - [?style] specifies whether to use a UTF-8 or an ASCII encoding for the
        progress bar. The UTF-8 encoding shows a higher resolution of progress,
        but may not be supported in all terminals. The default is [`ASCII].

      - [?width] is the width of the bar in columns. Defaults to [`Expand]. *)

  val bar_unaccumulated :
       ?style:[ `ASCII | `UTF8 | `Custom of string list ]
    -> ?color:color
    -> ?color_empty:color
    -> ?width:[ `Fixed of int | `Expand ]
    -> total:integer
    -> unit
    -> integer t
  (** TODO: better distinction here *)
end

module type S = sig
  type color
  type 'a printer

  type 'a t
  (** The type of segments of progress bars that display reported values of type
      ['a]. *)

  (** {1 Basic line segments} *)

  val const : string -> _ t
  (** [const s] is the segment that always displays [s]. *)

  val constf : ('a, Format.formatter, unit, _ t) format4 -> 'a
  (** [constf fmt a b c ...] is equivalent to
      [const (Format.asprintf fmt a b c ...)]. *)

  val string : string t

  val spinner :
       ?color:color
    -> ?frames:string list
    -> ?min_interval:Duration.t option
    -> unit
    -> _ t

  val basic : init:'a -> 'a printer -> 'a t
  (** TODO: Rename to [of_printer] and keep a distinction between accumulated
      printers. *)

  val elapsed : unit -> _ t
  (** Displays the time for which the bar has been rendering in [MM:SS] form. *)

  (** {1 Integer line segments} *)

  (** @inline *)
  include
    Integer_dependent
      with type 'a t := 'a t
       and type color := color
       and type 'a printer := 'a printer
       and type integer := int

  module type Integer_dependent = sig
    include
      Integer_dependent
        with type 'a t := 'a t
         and type color := color
         and type 'a printer := 'a printer
  end

  module Int32 : Integer_dependent with type integer := int32
  module Int64 : Integer_dependent with type integer := int64
  module Float : Integer_dependent with type integer := float

  module Integer_dependent (Integer : Integer.S) :
    Integer_dependent with type integer := Integer.t

  (** {1 Combining segments} *)

  val ( ++ ) : 'a t -> 'a t -> 'a t
  (** Horizontally join two segments of the same reported value type. *)

  val list : ?sep:'a t -> 'a t list -> 'a t
  (** Horizontally join a list of segments, with a given separator. [sep]
      defaults to [const " "]. *)

  val pair : ?sep:unit t -> 'a t -> 'b t -> ('a * 'b) t
  (** Horizontally join a pair of segments consuming different reported values
      into a single segment that consumes a pair. *)

  val lpad : int -> 'a t -> 'a t
  (** [lpad n t] left-pads the segment [t] to size [n] by adding blank space at
      the start. *)

  val rpad : int -> 'a t -> 'a t
  (** [rpad n t] right-pads the segment [t] to size [n] by adding blank space at
      the end. *)

  val using : ('a -> 'b) -> 'b t -> 'a t
  (** [using f s] is a segment that first applies [f] to the reported value and
      then behaves as segment [s]. *)

  val noop : unit -> _ t
  (** A zero-width line segment that does nothing. *)

  (** {1 Primitive line segment DSL} *)

  module Primitives : sig
    (** Provides the primitive ... *)

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

  module Platform_dependent (_ : Platform.S) : sig
    include
      S
        with type 'a t := 'a t
         and type color := Ansi.Color.t
         and type 'a printer := 'a Printer.t

    val compile : 'a t -> Config.t -> 'a Primitives.t
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
