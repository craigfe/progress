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

let add_substring t s offset len =
  if offset < 0 || len < 0 || offset > String.length s - len then
    invalid_arg "Line_buffer.add_substring";
  let position = t.position in
  advance t len;
  Bytes.unsafe_blit_string s offset t.buffer position len

let create ~size =
  let buffer = Bytes.create size in
  let rec ppf =
    lazy
      (let ppf = Format.make_formatter (add_substring t) (fun () -> ()) in
       Fmt.set_style_renderer ppf `Ansi_tty;
       ppf)
  and t = { buffer; position = 0; length = size; ppf } in
  t

let ppf t = Lazy.force t.ppf

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
