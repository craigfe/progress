module type Counter = sig
  type t

  val counter : unit -> t
  val count : t -> Mtime.span
end

(** The DSL of progress bar segments. *)
module type S = sig
  type 'a t
  (** The type of segments of progress bars that display reported values of type
      ['a]. *)

  (** {2 Pre-provided segments} *)

  val bar :
    mode:[ `ASCII | `UTF8 ] ->
    ?width:[ `Fixed of int | `Expand ] ->
    ('a -> float) ->
    'a t
  (** [bar ~width f] is a progress bar of the form:

      {[ [#######################################................] ]}

      which occupies [width]-many columns and uses [f] to determine the
      proportion of the bar that is filled.

      If [~width:`Expand] is passed – which is the default – this segment
      must be contained inside a {{!boxes} box} that determines its size. *)

  val const : string -> _ t
  (** [const s] is the segment that always displays [s]. [s] must not contain
      any newline characters. *)

  val const_fmt : width:int -> (Format.formatter -> unit) -> _ t
  (** {!const_fmt} is a variant of {!const} that takes a fixed-width
      pretty-printer rather than a string. *)

  val of_pp : width:int -> (Format.formatter -> 'a -> unit) -> 'a t
  (** [of_pp ~width pp] is a segment that uses the supplied fixed-width
      pretty-printer to render the value. The pretty-printer must never emit
      newline characters. *)

  (** {2:stateful Stateful segments} *)

  val periodic : int -> 'a t -> 'a t
  (** [periodic n s] has the same output format as [s], but only passes reported
      values down to [s] on every [n]-th call. This is useful when progress is
      being reported from a hot-loop, where the cost of rendering is
      non-negligible. *)

  val accumulator : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t
  (** [accumulator combine zero s] has the same output format [s]. *)

  val stateful : (unit -> 'a t) -> 'a t
  (** [stateful f] is a segment that behaves as [f ()] for any given render,
      allowing [f] to initialise any display state at the start of the rendering
      process. *)

  (** {2:boxes Dynamically-sized segments} *)

  (** Certain segments can have their size determined dynamically by being
      wrapped inside one of the following boxes: *)

  val box_dynamic : (unit -> int) -> 'a t -> 'a t
  (** [box w] is a box that wraps a dynamically-sized segment and sets it to
      have size [w ()] on each tick. *)

  val box_winsize : fallback:int -> 'a t -> 'a t
  (** A box that takes on the current size of the terminal (or [fallback] if
      stdout is not attached to a terminal.) *)

  val box_fixed : int -> 'a t -> 'a t
  (** [box-fixed n s] fixes the size of the dynamic segment [s] to be [n]. *)

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
end

module type Segment = sig
  module type S = S

  include S

  type 'a compiled

  val compile : initial:'a -> 'a t -> 'a compiled
  val update : 'a compiled -> Format.formatter -> unit
  val report : 'a compiled -> Format.formatter -> 'a -> unit
end
