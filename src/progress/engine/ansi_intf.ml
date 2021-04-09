(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

module type Style = sig
  (** Functions for building *)

  type t
  type color

  (** Constructing ANSI styles: *)

  val none : t
  val bold : t
  val faint : t
  val italic : t
  val underline : t
  val reverse : t
  val fg : color -> t
  val bg : color -> t

  val code : t -> string
  (** Get the ANSI escape code for the given style. *)
end

module type Ansi = sig
  module Color : sig
    type t

    type plain =
      [ `black | `blue | `cyan | `green | `magenta | `red | `white | `yellow ]

    val of_ansi : [ plain | `bright of plain ] -> t
    val of_hex : string -> t
    val of_rgb : int -> int -> int -> t
  end

  module type Style = Style

  module Style : Style with type color := Color.t

  val show_cursor : string
  val hide_cursor : string
  val move_up : int Fmt.t
  val move_down : int Fmt.t
  val erase_line : string
  val erase_display_suffix : string
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
