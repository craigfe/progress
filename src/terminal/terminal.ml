(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

module Color = Color
module Style = Style
module Ansi = Ansi

let guess_printed_width, truncate_to_width =
  Ansi.(guess_printed_width, truncate_to_width)

module Size = struct
  type dimensions = { rows : int; columns : int }

  external sigwinch : unit -> int option = "ocaml_terminal_get_sigwinch"

  external get_dimensions : unit -> dimensions option
    = "ocaml_terminal_get_terminal_dimensions"

  let get_rows () =
    match get_dimensions () with Some { rows; _ } -> Some rows | None -> None

  let get_columns () =
    match get_dimensions () with
    | Some { columns; _ } -> Some columns
    | None -> None

  let sigwinch = sigwinch ()
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
