(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

type plain =
  [ `black | `blue | `cyan | `green | `magenta | `red | `white | `yellow ]

type t = Ansi of [ plain | `bright of plain ] | Rgb of int * int * int

let pp_plain : plain Fmt.t =
  Fmt.of_to_string (function
    | `black -> "black"
    | `blue -> "blue"
    | `cyan -> "cyan"
    | `green -> "green"
    | `magenta -> "magenta"
    | `red -> "red"
    | `white -> "white"
    | `yellow -> "yellow")

let pp_dump ppf = function
  | Rgb (r, g, b) -> Fmt.pf ppf "RGB (%d, %d, %d)" r g b
  | Ansi (#plain as x) -> Fmt.pf ppf "ANSI (%a)" pp_plain x
  | Ansi (`bright x) -> Fmt.pf ppf "ANSI (bright %a)" pp_plain x

let ansi x = Ansi x

let rgb =
  let invalid_component typ n =
    Fmt.invalid_arg "Color.rgb: invalid %s component %d" typ n
  in
  fun r g b ->
    if r < 0 || r > 255 then invalid_component "red" r;
    if g < 0 || g > 255 then invalid_component "green" g;
    if b < 0 || b > 255 then invalid_component "blue" b;
    Rgb (r, g, b)

let hex =
  let invalid_length =
    Fmt.invalid_arg "Color.hex: invalid hexstring length %d"
  in
  let hex c =
    if c >= '0' && c <= '9' then Char.code c - Char.code '0'
    else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
    else if c >= 'A' && c <= 'F' then Char.code c - Char.code 'A' + 10
    else Fmt.invalid_arg "Color.hex: invalid hexstring character %c" c
  in
  fun s ->
    let len = String.length s in
    if len = 0 then invalid_length len;
    if s.[0] <> '#' then invalid_arg "Color.hex: hexstrings must start with '#'";
    let r1, r0, g1, g0, b1, b0 =
      match len with
      | 7 -> (hex s.[1], hex s.[2], hex s.[3], hex s.[4], hex s.[5], hex s.[6])
      | 4 ->
          (* Short hexstrings of the form #ABC alias longer ones of the form #AABBCC *)
          let r, g, b = (hex s.[1], hex s.[2], hex s.[3]) in
          (r, r, g, g, b, b)
      | _ -> invalid_length len
    in
    rgb ((16 * r1) + r0) ((16 * g1) + g0) ((16 * b1) + b0)

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
