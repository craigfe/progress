(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

let malformed_string s =
  Fmt.invalid_arg "Terminal.Ansi: malformed UTF-8 string: %S" s

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

let guess_printed_width s =
  let count = Length_counter.empty () in
  Uutf.String.fold_utf_8
    (fun () _ -> function
      | `Malformed _ -> malformed_string s
      | `Uchar c -> Length_counter.add count c)
    () s;
  Length_counter.count count

let uchar_size u =
  match Uchar.to_int u with
  | u when u < 0 -> assert false
  | u when u <= 0x007F -> 1
  | u when u <= 0x07FF -> 2
  | u when u <= 0xFFFF -> 3
  | u when u <= 0x10FFFF -> 4
  | _ -> assert false

let truncate_to_width width s =
  let count = Length_counter.empty () in
  let exception Exit of int in
  try
    Uutf.String.fold_utf_8
      (fun () i -> function
        | `Malformed _ -> malformed_string s
        | `Uchar c ->
            if Length_counter.count count = width then
              (* Check for display reset, and add it if it's there; truncating
                 this would cause the open colour to leak. *)
              let display_reset = "\027[0m" in
              if
                i + 4 < String.length s
                && String.equal (String.sub s ~pos:i ~len:4) display_reset
              then raise (Exit (i + 4))
              else raise (Exit i)
            else (
              Length_counter.add count c;
              let count = Length_counter.count count in
              if count <= width then () else raise (Exit i)))
      () s;
    s
  with Exit len -> String.sub s ~pos:0 ~len

let show_cursor = "\x1b[?25h"
let hide_cursor = "\x1b[?25l"
let erase_display_suffix = "\x1b[J"
let erase_line = "\x1b[K"
let move_up ppf = function 0 -> () | n -> Format.fprintf ppf "\x1b[%dA" n
let move_down ppf = function 0 -> () | n -> Format.fprintf ppf "\x1b[%dB" n

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
