(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

type user_supplied =
  { ppf : Format.formatter option
  ; hide_cursor : bool option
  ; persistent : bool option
  ; max_width : int option option
  ; min_interval : Duration.t option option
  }

module Default = struct
  let ppf =
    (* We avoid using [Format.err_formatter] directly since [Fmt] uses
       physical equality to share configuration options. *)
    let ppf = Format.formatter_of_out_channel stderr in
    Fmt.set_style_renderer ppf `Ansi_tty;
    Fmt.set_utf_8 ppf true;
    ppf

  let hide_cursor = true
  let persistent = true
  let max_width = None
  let min_interval = Some (Duration.of_sec (1. /. 60.))
end

(* Boilerplate from here onwards. Someday I'll write a PPX for this... *)

let create ?ppf ?hide_cursor ?persistent ?max_width ?min_interval () =
  { ppf; hide_cursor; persistent; max_width; min_interval }

(* Merge two ['a option]s with a left [Some] taking priority *)
let merge_on ~f a b = match (f a, f b) with Some a, _ -> Some a | None, b -> b

let ( || ) a b =
  { ppf = merge_on a b ~f:(fun x -> x.ppf)
  ; hide_cursor = merge_on a b ~f:(fun x -> x.hide_cursor)
  ; persistent = merge_on a b ~f:(fun x -> x.persistent)
  ; max_width = merge_on a b ~f:(fun x -> x.max_width)
  ; min_interval = merge_on a b ~f:(fun x -> x.min_interval)
  }

type t =
  { ppf : Format.formatter
  ; hide_cursor : bool
  ; persistent : bool
  ; max_width : int option
  ; min_interval : Duration.t option
  }

let apply_defaults : user_supplied -> t =
 fun { ppf; hide_cursor; persistent; max_width; min_interval } ->
  { ppf = Option.value ppf ~default:Default.ppf
  ; hide_cursor = Option.value hide_cursor ~default:Default.hide_cursor
  ; persistent = Option.value persistent ~default:Default.persistent
  ; max_width = Option.value max_width ~default:Default.max_width
  ; min_interval = Option.value min_interval ~default:Default.min_interval
  }

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
