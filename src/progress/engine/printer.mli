(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

type -'a t
(** The type of {i fixed-width} pretty-printers for values of type ['a].
    Specifically, these pretty-printers aim to emit strings that have the same
    {i rendered} length in terminals (i.e. after accounting for UTF-8 encoding
    and ANSI escapes).

    See {!Units} for a number of pre-provided printers for working with progress
    bars. *)

val int : width:int -> int t
(** [int ~width] pretty-prints integers using [width] characters, adding left
    padding if necessary. The printer will raise [Invalid_argument] if the
    integer to be printed can't be displayed in the given width. *)

val string : width:int -> string t
(** [string ~width] is a pretty-printer for UTF8-encoded strings using [width]
    characters, adding right padding or truncating with ellipses as necessary.

    For example, [string ~width:8] processes values as follows:

    {[
      ""            ↦ "        "
      "hello"       ↦ "hello   "
      "hello world" ↦ "hello..."
    ]}

    {b Note:}
    {i this printer uses a heuristic function ({!Uucp.tty_width_hint}) to guess
       the rendered length of supplied strings. This function is not guaranteed
       to be correct on all UTF08 codepoints, and so certain "unusual" string
       inputs can cause progress bar rendering to go awry.} *)

(** {1 Consuming printers} *)

val to_pp : 'a t -> Format.formatter -> 'a -> unit
(** Convert a pretty-printer to a [Format]-compatible pretty-printer. *)

val to_to_string : 'a t -> 'a -> string
(** Convert a pretty-printer to a [to_string] function. *)

val print_width : _ t -> int
(** [print_width t] is the number of terminal columns occupied by the output of
    [t]. *)

(** {1 Constructing printers} *)

val of_to_string : len:int -> ('a -> string) -> 'a t
val using : f:('b -> 'a) -> 'a t -> 'b t
val create : to_string:('a -> string) -> string_len:int -> pp:'a pp -> 'a t

(** {1 Internals} *)

module Internals : sig
  val integer : width:int -> (module Integer.S with type t = 'a) -> 'a t
  val to_line_printer : 'a t -> (Line_buffer.t -> 'a -> unit) Staged.t
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
