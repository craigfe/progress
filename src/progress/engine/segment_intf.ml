open! Import

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

  type theta := Line_buffer.t -> unit
  type 'a alpha := Line_buffer.t -> 'a -> unit

  val noop : unit -> _ t
  val theta : width:int -> theta -> _ t
  val alpha : width:int -> initial:theta -> 'a alpha -> 'a t

  val alpha_unsized :
       initial:(width:(unit -> int) -> Line_buffer.t -> int)
    -> (width:(unit -> int) -> Line_buffer.t -> 'a -> int)
    -> 'a t

  val array : 'a t array -> 'a t
  val pair : ?sep:unit t -> 'a t -> 'b t -> ('a * 'b) t
  val contramap : f:('a -> 'b) -> 'b t -> 'a t

  val of_pp :
       width:int
    -> initial:(Format.formatter -> unit)
    -> (Format.formatter -> 'a -> unit)
    -> 'a t
  (** [of_pp ~width pp] is a segment that uses the supplied fixed-width
      pretty-printer to render the value. The pretty-printer must never emit
      newline characters. *)

  val conditional : ('a -> bool) -> 'a t -> 'a t
  (** [conditional pred s] has the same output format as [s], but is only passes
      reported values down to [s] when they satisfy [pred]. *)

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

  val box_fixed : int -> 'a t -> 'a t
  (** [box-fixed n s] fixes the size of the dynamic segment [s] to be [n]. *)

  module Platform_dependent (_ : Platform.S) : sig
    val box_winsize : ?max:int -> ?fallback:int -> 'a t -> 'a t
    (** A box that takes on the current size of the terminal (or [fallback] if
        stdout is not attached to a terminal).

        @param fallback defaults to [80].
        @param max defaults to no limit. *)
  end
end

module type Segment = sig
  module type S = S

  include S

  module Compiled : sig
    type 'a t

    val pp_dump : Format.formatter -> 'a t -> unit
  end

  val compile : 'a t -> 'a Compiled.t

  val update :
    'a Compiled.t -> (unconditional:bool -> Line_buffer.t -> int) Staged.t

  val report : 'a Compiled.t -> (Line_buffer.t -> 'a -> int) Staged.t
end
