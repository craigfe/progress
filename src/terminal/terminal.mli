(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(** This library provides a small set of standard utility functions for
    interacting with terminals. *)

module Color : sig
  type t
  (** The type of colours that can be rendered to a terminal. *)

  (** {1 4-bit ANSI colours}

      Colours built using {!ansi} will be rendered using the standard
      {{:https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit} 4-bit
        ANSI escape codes} for terminals. The actual colours displayed to the
      user depend on their terminal configuration / theme, ensuring that they
      look natural in context. *)

  type plain =
    [ `black | `blue | `cyan | `green | `magenta | `red | `white | `yellow ]

  val ansi : [ plain | `bright of plain ] -> t

  (** {1 24-bit RGB colours}

      Most modern terminals offer support for full 24-bit RGB colour (called
      "{{:https://en.wikipedia.org/wiki/ANSI_escape_code#24-bit} true colour}")
      in addition to the 16 original ANSI colours. These colours are rendered
      exactly as requested, offering greater flexibility at the risk of clashing
      with the user's theming. *)

  val rgb : int -> int -> int -> t
  (** [rgb r g b] is the RGB24 colour with the given red, green and blue colour
      components respectively. Raises [Invalid_argument] if any of the
      components are outside the range [\[0, 255\]]. *)

  val hex : string -> t
  (** [hex s] is the RGB24 colour given by the
      {{:https://en.wikipedia.org/wiki/Web_colors#Hex_triplet} hex triplet} [s],
      which must start with [#]. Examples:

      - [hex "#FF8C00"] = [rgb 0xFF 0x8C 0x00]
      - [hex "#fa0"] = [rgb 0xFF 0xAA 0x00]

      Raises [Invalid_argument] if the given string is not a [#]-prefixed hex
      triplet. *)

  val pp_dump : Format.formatter -> t -> unit
  (** Pretty-print a colour with an unspecified format. *)
end

module Style : sig
  type t
  (** The type of terminal {i styles}: values that can be printed to a terminal
      in order to change the way that it renders text. *)

  val code : t -> string
  (** Get the ANSI escape code for the given style. *)

  (** Constructing ANSI styles: *)

  val none : t
  val bold : t
  val faint : t
  val italic : t
  val underline : t
  val reverse : t
  val fg : Color.t -> t
  val bg : Color.t -> t
end

module Ansi : sig
  val show_cursor : string
  val hide_cursor : string
  val move_up : Format.formatter -> int -> unit
  val move_down : Format.formatter -> int -> unit
  val erase_line : string
  val erase_display_suffix : string
end

module Size : sig
  val sigwinch : int option
  (** The number of the signal used to indicate terminal size changes. [None] on
      Windows. *)

  (** Functions for getting the size of the terminal to which [stdout] is
      attached (provided [stdout] is a TTY). *)

  type dimensions = { rows : int; columns : int }

  val get_dimensions : unit -> dimensions option
  val get_columns : unit -> int option
  val get_rows : unit -> int option
end

val guess_printed_width : string -> int
(** [guess_printed_width s] returns an estimate of the number of terminal
    columns that the UTF-8 encoded string [s] would occupy if displayed in a
    terminal, after stripping any ANSI escape codes in the string.

    {b Note:}
    {i this function uses a heuristic ({!Uucp.tty_width_hint}) to guess the
       rendered length of supplied strings. This function is not guaranteed to
       be correct on all UTF-8 codepoints. See the [Uucp] documentation for
       details.} *)

val truncate_to_width : int -> string -> string
(** [truncate_to_width n s] is the longest prefix of UTF-8 encoded string [s]
    that will fit within [n] columns when displayed in a terminal (without
    including unbalanced ANSI control sequences after the [n]-th column).

    As with {!guess_printed_width}, the implementation relies on heuristics and
    so may not be accurate for all inputs (or for all terminal implementations).*)

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
