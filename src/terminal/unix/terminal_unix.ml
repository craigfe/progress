(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

external sigwinch : unit -> int option = "ocaml_terminal_get_sigwinch"
(** The number of the signal used to indicate terminal size changes. [None] on
    Windows. *)

type dimensions = { rows : int; columns : int }

external get_dimensions : unit -> dimensions option
  = "ocaml_terminal_get_terminal_dimensions"

let get_rows () =
  match get_dimensions () with Some { rows; _ } -> Some rows | None -> None

let get_columns () =
  match get_dimensions () with
  | Some { columns; _ } -> Some columns
  | None -> None

let on_change = ref (fun _ -> ())

let initialise =
  let handle_signal _ = !on_change () in
  lazy
    (match sigwinch () with
    | None -> ()
    | Some n -> Sys.set_signal n (Signal_handle handle_signal))

let set_changed_callback f =
  Lazy.force initialise;
  on_change := f

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
