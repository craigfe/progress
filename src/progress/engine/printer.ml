(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

(** TODO: keep distinction between display columns and string length. *)

external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string"
  [@@noalloc]
(** Polyfill for pre-4.09.0 *)

type 'a t =
  { write : 'a -> into:bytes -> pos:int -> unit
  ; write_len : int
  ; to_string : 'a -> string
  ; pp : 'a pp
  }

let create ?pp ~to_string ~string_len () =
  let write x ~into ~pos =
    unsafe_blit_string (to_string x) 0 into pos string_len
  in
  let pp = match pp with None -> Fmt.of_to_string to_string | Some pp -> pp in
  { write; write_len = string_len; to_string; pp }

(** TODO: handle overflows *)

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

let malformed_string s =
  Fmt.invalid_arg "Printer.string: malformed UTF-8 string: %S" s

module Length_counter = struct
  (* Counting length of UTF-8 strings while skipping ANSI escape sequences. See
      https://en.wikipedia.org/wiki/ANSI_escape_code#Fe_Escape_sequences for
      details. *)
  type t =
    { mutable acc : int
    ; mutable state :
        [ `Normal
        | `Parsing_ansi_sequence (* Read '\x1b', but not the subsequent byte. *)
        | `Ansi_parameter_bytes (* Inside a CSI parameter sequence *) ]
    }

  let empty () = { acc = 0; state = `Normal }
  let is_initial_ansi_byte c = Char.equal c '\x1b'

  let is_final_ansi_byte c =
    let c = Char.code c in
    c >= 0x40 && c <= 0x7e

  let guess_printed_char_length c =
    match Uucp.Break.tty_width_hint c with
    | -1 -> 1 (* Assume width of 1 if [Uucp] can't guess *)
    | n -> n

  let add t c =
    match Uchar.is_char c with
    | false -> t.acc <- t.acc + guess_printed_char_length c
    | true -> (
        let c = Uchar.to_char c in
        match t.state with
        | `Normal ->
            if is_initial_ansi_byte c then t.state <- `Parsing_ansi_sequence
            else t.acc <- t.acc + 1
        | `Parsing_ansi_sequence ->
            if Char.equal c '[' (* Control sequence introducer *) then
              t.state <- `Ansi_parameter_bytes
            else t.state <- `Normal
        | `Ansi_parameter_bytes ->
            if is_final_ansi_byte c then t.state <- `Normal)

  let count t = t.acc
end

let guess_printed_string_length s =
  let count = Length_counter.empty () in
  Uutf.String.fold_utf_8
    (fun () _ -> function
      | `Malformed _ -> malformed_string s
      | `Uchar c -> Length_counter.add count c)
    () s;
  Length_counter.count count

let truncate_to_width width s =
  Fmt.epr "Truncating %S to width %d@." s width;
  let buf = Buffer.create width in
  let count = Length_counter.empty () in
  let exception Exit in
  (try
     Uutf.String.fold_utf_8
       (fun () i -> function
         | `Malformed _ -> malformed_string s
         | `Uchar c ->
             if Length_counter.count count = width then (
               (* Check for display reset, and add it if it's there; truncating
                  this would cause the open colour to leak. *)
               let display_reset = "\027[0m" in
               if
                 i + 4 < String.length s
                 && String.equal (String.sub s ~pos:i ~len:4) display_reset
               then Buffer.add_string buf display_reset;
               raise Exit)
             else (
               Length_counter.add count c;
               let count = Length_counter.count count in
               if count <= width then Buffer.add_utf_8_uchar buf c
               else raise Exit))
       () s
   with Exit -> ());
  Fmt.epr "Returning: %S@." (Buffer.contents buf);
  buf

let string ~width =
  let ellipsis_length = min 3 width in
  let ellipsis = String.make ellipsis_length '.' in
  let to_string s =
    let printed_len = guess_printed_string_length s in
    let padding = width - printed_len in
    if padding = 0 then s
    else if padding > 0 then (
      let len = String.length s in
      let buf = Bytes.make (len + padding) ' ' in
      unsafe_blit_string s 0 buf 0 len;
      Bytes.unsafe_to_string buf)
    else
      let buf = truncate_to_width (width - ellipsis_length) s in
      Buffer.add_string buf ellipsis;
      Buffer.contents buf
  in
  create ~string_len:width ~to_string ()

let to_pp { pp; _ } = pp

let using ~f { write; write_len; to_string; pp } =
  let write x ~into ~pos = write (f x) ~into ~pos in
  let pp = Fmt.using f pp in
  let to_string x = to_string (f x) in
  { write; write_len; to_string; pp }

let to_line_printer { write; write_len; _ } =
  Line_buffer.lift_write ~len:write_len ~write

let to_to_string { to_string; _ } = to_string
let print_width { write_len; _ } = write_len

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
