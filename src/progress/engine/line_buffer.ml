(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

external unsafe_blit_string : string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string"
  [@@noalloc]
(** Polyfill for pre-4.09.0 *)

type t =
  { mutable buffer : bytes
  ; mutable position : int
  ; mutable length : int
  ; ppf : Format.formatter Lazy.t
  }
(** Invariants:

    - [0 <= position <= length]
    - [length = Bytes.length buffer] *)

let resize t more =
  let old_pos = t.position and old_len = t.length in
  let new_len =
    let res = ref old_len in
    while old_pos + more > !res do
      res := 2 * !res
    done;
    !res
  in
  let new_buffer = Bytes.create new_len in
  Bytes.blit t.buffer 0 new_buffer 0 t.position;
  t.buffer <- new_buffer;
  t.length <- new_len

let advance t len =
  let new_position = t.position + len in
  if new_position > t.length then resize t len;
  (* Fmt.pr "[%d -> %d]" t.position new_position; *)
  t.position <- new_position

let lift_write ~len ~write =
  Staged.inj (fun t x ->
      let position = t.position in
      advance t len;
      write x ~into:t.buffer ~pos:position)

let add_char b c =
  let pos = b.position in
  if pos >= b.length then resize b 1;
  Bytes.unsafe_set b.buffer pos c;
  b.position <- pos + 1

let add_substring t s ~off ~len =
  if off < 0 || len < 0 || off > String.length s - len then
    invalid_arg "Line_buffer.add_substring";
  let position = t.position in
  advance t len;
  unsafe_blit_string s off t.buffer position len

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  unsafe_blit_string s 0 b.buffer b.position len;
  b.position <- new_position

let add_line_buffer ~dst ~src =
  let position = dst.position in
  let len = src.position in
  advance dst len;
  Bytes.unsafe_blit src.buffer 0 dst.buffer position len

let create ~size =
  let buffer = Bytes.create size in
  let rec ppf =
    lazy
      (let ppf =
         Format.make_formatter
           (fun s off len -> add_substring t s ~off ~len)
           (fun () -> ())
       in
       Fmt.set_style_renderer ppf `Ansi_tty;
       ppf)
  and t = { buffer; position = 0; length = size; ppf } in
  t

let with_ppf t f =
  let ppf = Lazy.force t.ppf in
  let a = f ppf in
  Format.pp_print_flush ppf ();
  a

let reset t = t.position <- 0

let contents t =
  let res = Bytes.sub_string t.buffer 0 t.position in
  reset t;
  res

type mark = int

let current_position t = t.position

module Span = struct
  type t = { pos : int; len : int }

  let pp ppf t = Fmt.pf ppf "{ pos = %d; len = %d }" t.pos t.len
  let empty = { pos = 0; len = 0 }
  let between_marks a b = { pos = a; len = b - a }
end

let skip t (span : Span.t) =
  (* XXX: this can cause spurious failures when zooming the terminal, so for the
     moment we don't validate positions whatsoever. *)
  (* if t.position <> span.pos then
   *   Fmt.failwith "Misaligned span %a inside line buffer at position %d" Span.pp
   *     span t.position; *)
  advance t span.len

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
