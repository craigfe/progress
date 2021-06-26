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

    {b Most users don't need this module, and can use the pre-provided {!Line}
       segments and {!Units} printers directly.} *)

(** {1 Constructing printers} *)

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
       to be correct on all UTF-8 codepoints, and so certain "unusual" string
       inputs can cause progress bar rendering to go awry.} *)

val using : f:('b -> 'a) -> 'a t -> 'b t
(** [using ~f t] prints values [v] by passing [f v] to the printer [t]. *)

val create :
     ?width:int
  -> ?pp:'a pp
  -> to_string:('a -> string)
  -> string_len:int
  -> unit
  -> 'a t
(** [create ~to_string ~string_len ()] is a printer that uses [to_string] to
    render values in [string_len]-many bytes.

    The rendered width of the output (when displayed in a terminal) is assumed
    to also be [string_len] (i.e. the output string is assumed to be ASCII), but
    this can be assumption can be overridden by passing an explicit [~width]
    (e.g. if the printer emits non-ASCII UTF-8 characters or ANSI escape
    sequences). *)

(** {1 Consuming printers} *)

val to_pp : 'a t -> Format.formatter -> 'a -> unit
(** Convert a pretty-printer to a [Format]-compatible pretty-printer. *)

val to_to_string : 'a t -> 'a -> string
(** Convert a pretty-printer to a [to_string] function. *)

val print_width : _ t -> int
(** [print_width t] is the number of terminal columns occupied by the output of
    [t]. *)

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
