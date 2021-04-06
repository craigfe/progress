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

let create ~to_string ~string_len ~pp =
  let write x ~into ~pos =
    unsafe_blit_string (to_string x) 0 into pos string_len
  in
  { write; write_len = string_len; to_string; pp }

let of_to_string ~len to_string =
  create ~to_string ~string_len:len ~pp:(Fmt.of_to_string to_string)

(** TODO: handle overflows *)

let int ~width = of_to_string ~len:width Int.to_string
let string ~width = of_to_string ~len:width Fun.id
let to_pp { pp; _ } = pp

let using ~f { write; write_len; to_string; pp } =
  let write x ~into ~pos = write (f x) ~into ~pos in
  let pp = Fmt.using f pp in
  let to_string x = to_string (f x) in
  { write; write_len; to_string; pp }

let to_to_string { to_string; _ } = to_string

let to_line_printer { write; write_len; _ } =
  Line_buffer.lift_write ~len:write_len ~write

let width { write_len; _ } = write_len
