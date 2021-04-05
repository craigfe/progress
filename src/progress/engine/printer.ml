(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

type 'a t =
  { write : 'a -> into:bytes -> pos:int -> unit; write_len : int; pp : 'a pp }

let create ~to_string ~string_len ~pp =
  let write x ~into ~pos =
    Bytes.unsafe_blit_string (to_string x) 0 into pos string_len
  in
  { write; write_len = string_len; pp }

let of_to_string ~len to_string =
  create ~to_string ~string_len:len ~pp:(Fmt.of_to_string to_string)

let to_pp { pp; _ } = pp

let using ~f { write; write_len; pp } =
  let write x ~into ~pos = write (f x) ~into ~pos in
  let pp = Fmt.using f pp in
  { write; write_len; pp }

let to_line_printer { write; write_len; _ } =
  Line_buffer.lift_write ~len:write_len ~write

let width { write_len; _ } = write_len
