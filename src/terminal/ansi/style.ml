(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

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
  | Fg (Ansi `black) -> "\x1b[30m"
  | Fg (Ansi `red) -> "\x1b[31m"
  | Fg (Ansi `green) -> "\x1b[32m"
  | Fg (Ansi `yellow) -> "\x1b[33m"
  | Fg (Ansi `blue) -> "\x1b[34m"
  | Fg (Ansi `magenta) -> "\x1b[35m"
  | Fg (Ansi `cyan) -> "\x1b[36m"
  | Fg (Ansi `white) -> "\x1b[37m"
  | Bg (Ansi `black) -> "\x1b[40m"
  | Bg (Ansi `red) -> "\x1b[41m"
  | Bg (Ansi `green) -> "\x1b[42m"
  | Bg (Ansi `yellow) -> "\x1b[43m"
  | Bg (Ansi `blue) -> "\x1b[44m"
  | Bg (Ansi `magenta) -> "\x1b[45m"
  | Bg (Ansi `cyan) -> "\x1b[46m"
  | Bg (Ansi `white) -> "\x1b[47m"
  | Fg (Ansi (`bright `black)) -> "\x1b[90m"
  | Fg (Ansi (`bright `red)) -> "\x1b[91m"
  | Fg (Ansi (`bright `green)) -> "\x1b[92m"
  | Fg (Ansi (`bright `yellow)) -> "\x1b[93m"
  | Fg (Ansi (`bright `blue)) -> "\x1b[94m"
  | Fg (Ansi (`bright `magenta)) -> "\x1b[95m"
  | Fg (Ansi (`bright `cyan)) -> "\x1b[96m"
  | Fg (Ansi (`bright `white)) -> "\x1b[97m"
  | Bg (Ansi (`bright `black)) -> "\x1b[100m"
  | Bg (Ansi (`bright `red)) -> "\x1b[101m"
  | Bg (Ansi (`bright `green)) -> "\x1b[102m"
  | Bg (Ansi (`bright `yellow)) -> "\x1b[103m"
  | Bg (Ansi (`bright `blue)) -> "\x1b[104m"
  | Bg (Ansi (`bright `magenta)) -> "\x1b[105m"
  | Bg (Ansi (`bright `cyan)) -> "\x1b[106m"
  | Bg (Ansi (`bright `white)) -> "\x1b[107m"
  | Fg (Rgb (r, g, b)) -> Printf.sprintf "\x1b[38;2;%d;%d;%dm" r g b
  | Bg (Rgb (r, g, b)) -> Printf.sprintf "\x1b[48;2;%d;%d;%dm" r g b

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
