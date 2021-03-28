type t =
  { mutable buffer : bytes
  ; mutable position : int
  ; mutable length : int
  ; ppf : Format.formatter Lazy.t
  }
(** Invariants:

    - [0 <= position <= length]
    - [length = Bytes.length buffer]*)

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
  t.position <- new_position

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
  Bytes.unsafe_blit_string s off t.buffer position len

let add_string b s =
  let len = String.length s in
  let new_position = b.position + len in
  if new_position > b.length then resize b len;
  Bytes.unsafe_blit_string s 0 b.buffer b.position len;
  b.position <- new_position

let add_style_code buf style = add_string buf (Ansi.style_code style)

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

let contents t =
  let res = Bytes.sub_string t.buffer 0 t.position in
  t.position <- 0;
  res

type mark = int

let current_position t = t.position

module Span = struct
  type t = { pos : int; len : int }

  let pp ppf t = Fmt.pf ppf "{ pos = %d; len = %d }" t.pos t.len
  let empty = { pos = 0; len = 0 }
  let between_marks a b = { pos = a; len = b - a + 1 }
end

let skip t (span : Span.t) =
  if t.position <> span.pos then
    Fmt.failwith "Misaligned span %a inside line buffer at position %d" Span.pp
      span t.position;
  advance t span.len
