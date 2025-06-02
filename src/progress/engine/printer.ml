(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string"
[@@noalloc]
(** Polyfill for pre-4.09.0 *)

type 'a t =
  { write : 'a -> into:bytes -> pos:int -> unit
  ; write_len : int
  ; width : int
  ; to_string : 'a -> string
  ; pp : 'a pp
  }

let create ?width ?pp ~to_string ~string_len () =
  let write x ~into ~pos =
    unsafe_blit_string (to_string x) 0 into pos string_len
  in
  let width = match width with None -> string_len | Some width -> width in
  let pp = match pp with None -> Fmt.of_to_string to_string | Some pp -> pp in
  { write; write_len = string_len; width; to_string; pp }

let integer (type a) ~width (module Integer : Integer.S with type t = a) : a t =
  let to_string x =
    let x = Integer.to_string x in
    let x_len = String.length x in
    let padding = width - x_len in
    if padding < 0 then
      Fmt.invalid_arg
        "Progress.Printer.int: can't print integer %s within a width of %d" x
        width;
    if padding = 0 then x
    else
      let buf = Bytes.make width ' ' in
      unsafe_blit_string x 0 buf padding x_len;
      Bytes.unsafe_to_string buf
  in
  create ~string_len:width ~to_string ()

let int ~width = integer ~width (module Integer.Int)

let string ~width =
  if width < 0 then failwith "Printer.string: negative print length";
  let ellipsis_length = min 3 width in
  let ellipsis = String.make ellipsis_length '.' in
  let to_string s =
    let printed_len = Terminal.guess_printed_width s in
    let padding = width - printed_len in
    if padding = 0 then s
    else if padding > 0 then (
      let len = String.length s in
      let buf = Bytes.make (len + padding) ' ' in
      unsafe_blit_string s 0 buf 0 len;
      Bytes.unsafe_to_string buf)
    else
      let s = Terminal.truncate_to_width (width - ellipsis_length) s in
      s ^ ellipsis
  in
  create ~string_len:width ~to_string ()

let to_pp { pp; _ } = pp

let using ~f { write; write_len; to_string; pp; width } =
  let write x ~into ~pos = write (f x) ~into ~pos in
  let pp = Fmt.using f pp in
  let to_string x = to_string (f x) in
  { write; write_len; to_string; pp; width }

let to_line_printer { write; write_len; _ } =
  Line_buffer.lift_write ~len:write_len ~write

let to_to_string { to_string; _ } = to_string
let print_width { width; _ } = width

module Internals = struct
  let integer = integer
  let to_line_printer = to_line_printer
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
