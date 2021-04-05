val show_cursor : string
val hide_cursor : string
val move_up : int Fmt.t
val move_down : int Fmt.t
val erase_line : string
val erase_display_suffix : string

module Color : sig
  type t

  type plain =
    [ `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ]

  val of_ansi : [ plain | `Bright of plain ] -> t
  val of_hex : string -> t
  val of_rgb : int -> int -> int -> t
end

module Style : sig
  type t

  (** Constructing ANSI styles: *)

  val none : t
  val bold : t
  val faint : t
  val italic : t
  val underline : t
  val reverse : t
  val fg : Color.t -> t
  val bg : Color.t -> t

  val code : t -> string
  (** Get the ANSI escape code for the given style. *)
end
