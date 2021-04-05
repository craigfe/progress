let show_cursor = "\x1b[?25h"
let hide_cursor = "\x1b[?25l"
let erase_display_suffix = "\x1b[J"
let erase_line = "\x1b[K"
let move_up ppf = function 0 -> () | n -> Format.fprintf ppf "\x1b[%dA" n
let move_down ppf = function 0 -> () | n -> Format.fprintf ppf "\x1b[%dB" n

module Color = struct
  type plain =
    [ `Black | `Blue | `Cyan | `Green | `Magenta | `Red | `White | `Yellow ]

  type t = Ansi of [ plain | `Bright of plain ] | Rgb of int * int * int

  let of_ansi x = Ansi x
  let of_rgb r g b = Rgb (r, g, b)

  let of_hex s =
    let len = String.length s in
    if len = 4 || len = 7 then
      let short = len = 4 in
      let r' = if short then String.sub s 1 1 else String.sub s 1 2 in
      let g' = if short then String.sub s 2 1 else String.sub s 3 2 in
      let b' = if short then String.sub s 3 1 else String.sub s 5 2 in
      let r = int_of_string_opt ("0x" ^ r') in
      let g = int_of_string_opt ("0x" ^ g') in
      let b = int_of_string_opt ("0x" ^ b') in
      match (r, g, b) with
      | Some r, Some g, Some b ->
          if short then of_rgb ((16 * r) + r) ((16 * g) + g) ((16 * b) + b)
          else of_rgb r g b
      | _ -> failwith "Invalid"
    else failwith "Incorrect length: %d"
end

module Style = struct
  type t =
    | None
    | Bold
    | Faint
    | Italic
    | Underline
    | Reverse
    | Fg of Color.t
    | Bg of Color.t

  let none = None
  let bold = Bold
  let faint = Faint
  let italic = Italic
  let underline = Underline
  let reverse = Reverse
  let fg x = Fg x
  let bg x = Bg x

  let code : t -> string = function
    | None -> "\x1b[0m"
    | Bold -> "\x1b[1m"
    | Faint -> "\x1b[2m"
    | Italic -> "\x1b[3m"
    | Underline -> "\x1b[4m"
    | Reverse -> "\x1b[7m"
    | Fg (Ansi `Black) -> "\x1b[30m"
    | Fg (Ansi `Red) -> "\x1b[31m"
    | Fg (Ansi `Green) -> "\x1b[32m"
    | Fg (Ansi `Yellow) -> "\x1b[33m"
    | Fg (Ansi `Blue) -> "\x1b[34m"
    | Fg (Ansi `Magenta) -> "\x1b[35m"
    | Fg (Ansi `Cyan) -> "\x1b[36m"
    | Fg (Ansi `White) -> "\x1b[37m"
    | Bg (Ansi `Black) -> "\x1b[40m"
    | Bg (Ansi `Red) -> "\x1b[41m"
    | Bg (Ansi `Green) -> "\x1b[42m"
    | Bg (Ansi `Yellow) -> "\x1b[43m"
    | Bg (Ansi `Blue) -> "\x1b[44m"
    | Bg (Ansi `Magenta) -> "\x1b[45m"
    | Bg (Ansi `Cyan) -> "\x1b[46m"
    | Bg (Ansi `White) -> "\x1b[47m"
    | Fg (Ansi (`Bright `Black)) -> "\x1b[90m"
    | Fg (Ansi (`Bright `Red)) -> "\x1b[91m"
    | Fg (Ansi (`Bright `Green)) -> "\x1b[92m"
    | Fg (Ansi (`Bright `Yellow)) -> "\x1b[93m"
    | Fg (Ansi (`Bright `Blue)) -> "\x1b[94m"
    | Fg (Ansi (`Bright `Magenta)) -> "\x1b[95m"
    | Fg (Ansi (`Bright `Cyan)) -> "\x1b[96m"
    | Fg (Ansi (`Bright `White)) -> "\x1b[97m"
    | Bg (Ansi (`Bright `Black)) -> "\x1b[100m"
    | Bg (Ansi (`Bright `Red)) -> "\x1b[101m"
    | Bg (Ansi (`Bright `Green)) -> "\x1b[102m"
    | Bg (Ansi (`Bright `Yellow)) -> "\x1b[103m"
    | Bg (Ansi (`Bright `Blue)) -> "\x1b[104m"
    | Bg (Ansi (`Bright `Magenta)) -> "\x1b[105m"
    | Bg (Ansi (`Bright `Cyan)) -> "\x1b[106m"
    | Bg (Ansi (`Bright `White)) -> "\x1b[107m"
    | Fg (Rgb (r, g, b)) -> Printf.sprintf "\x1b[38;2;%d;%d;%dm" r g b
    | Bg (Rgb (r, g, b)) -> Printf.sprintf "\x1b[48;2;%d;%d;%dm" r g b
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
