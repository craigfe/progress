(** The DSL of progress bar segments. **)
module type S = sig
  type 'a t
  (** The type of segments of progress bars that display reported values of type
      ['a]. *)

  (** {2 Pre-provided segments} *)

  val time : _ t
  (** Displays the time for which the bar has been rendering in [MM:SS] form. *)

  val percentage : float t

  val bar :
    mode:[ `UTF | `ASCII ] ->
    ?width:[ `Fixed of int | `Expand ] ->
    ('a -> float) ->
    'a t
  (** [bar width f] is a progress bar of the form:

      [\[#######################################................\] 71%]

      which occupies [width]-many columns and uses [f] to determine the
      proportion of the bar that is filled. *)

  val const : string -> _ t
  (** [const s] is the segment that always displays [s].

      @raise Invalid_arg if [s] contains any newlines. *)

  val fmt : (Format.formatter -> 'a -> unit) * int -> 'a t
  (** [fmt pp] is a segment that uses the supplied fixed-width pretty-printer to
      render the value. The formatter should ensure never to emit newlines. *)

  val periodic : int -> 'a t -> 'a t
  (** [periodic n s] has the same output format as [s], but only updates on
      every [n]-th call. This is useful when progress is being reported from a
      hot-loop, where the cost of rendering is non-negligible. *)

  val accumulator : ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t
  (** [accumulator combine zero s] is a *)

  (** {2 Dynamically-sized segments} *)

  val box_fixed : int -> 'a t -> 'a t
  val box_dynamic : int ref -> 'a t -> 'a t
  val box_winsize : 'a t -> 'a t

  (** {2 Combining segments} *)

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** Horizontally join two segments of the same reported value type. *)

  val list : ?sep:string -> 'a t list -> 'a t
  (** Horizontally join a list of segments, with a given separator. [sep]
      defaults to [" "]. *)

  val pair : ?sep:unit t -> 'a t -> 'b t -> ('a * 'b) t
  (** Horizontally join a pair of segments consuming different reported values
      into a single segment that consumes a pair. *)

  val contramap : ('a -> 'b) -> 'b t -> 'a t
  (** [contramap f s] is a segment that first applies [f] to the reported value
      and then behaves as segment [s]. *)
end

module type Segment = sig
  module type S = S

  include S

  type 'a unstaged

  val unstage : 'a t -> 'a unstaged

  (* val update : 'a unstaged -> Format.formatter -> unit *)
  val report : 'a unstaged -> Format.formatter -> 'a -> unit
end
