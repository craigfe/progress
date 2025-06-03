module Dynlist : sig
  type 'a t

  val of_list : 'a list -> 'a t
  val length : 'a t -> int
  val pop_opt : 'a t -> 'a option
end

val colour_picker : unit -> unit -> Progress.Color.t
