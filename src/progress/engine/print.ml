open! Import

type 'a t = { write : 'a -> into:bytes -> pos:int -> unit; len : int }

let of_to_string ~len to_string =
  let write x ~into ~pos =
    Bytes.unsafe_blit_string (to_string x) 0 into pos len
  in
  { write; len }

let to_line_buffer { len; write } = Line_buffer.lift_write ~len ~write
