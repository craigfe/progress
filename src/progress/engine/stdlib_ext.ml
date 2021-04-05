(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

type 'a pp = Format.formatter -> 'a -> unit

let ( >> ) f g x = g (f x)

let trace fmt x =
  Fmt.epr fmt x;
  x

module String = struct
  include String

  module Utf8 = struct
    let length = Uutf.String.fold_utf_8 (fun count _ _ -> count + 1) 0
  end
end

module Option = struct
  include Option

  let ( || ) a b = match a with Some _ -> a | None -> b
end

module Result = struct
  let get_or_invalid_arg = function
    | Ok x -> x
    | Error (`Msg s) -> invalid_arg s

  let errorf fmt = Format.kasprintf (fun s -> Error (`Msg s)) fmt
end

module List = struct
  include List

  let rec intersperse ~sep = function
    | ([] | [ _ ]) as l -> l
    | h1 :: (_ :: _ as tl) -> h1 :: sep :: intersperse ~sep tl
end

module Staged : sig
  type 'a t

  external inj : 'a -> 'a t = "%identity"
  external prj : 'a t -> 'a = "%identity"

  module Syntax : sig
    val ( let$ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and$ ) : 'a t -> 'b t -> ('a * 'b) t
  end
end = struct
  type 'a t = 'a

  external inj : 'a -> 'a t = "%identity"
  external prj : 'a t -> 'a = "%identity"

  module Syntax = struct
    let ( let$ ) x f = f x
    let ( and$ ) a b = (a, b)
  end
end

module Sta_dyn : sig
  type 'a t = Static of 'a | Dynamic of (unit -> 'a)

  val get : 'a t -> 'a
  val lift : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val pp : 'a pp -> 'a t pp
end = struct
  type 'a t = Static of 'a | Dynamic of (unit -> 'a)

  let get = function Static x -> x | Dynamic f -> f ()

  let lift add x y =
    let ( ++ ) = add in
    match (x, y) with
    | Static x, Static y -> Static (x ++ y)
    | Dynamic f, Static x | Static x, Dynamic f -> Dynamic (fun () -> x ++ f ())
    | Dynamic f, Dynamic g -> Dynamic (fun () -> f () ++ g ())

  let pp pp_elt ppf = function
    | Static x -> Fmt.pf ppf "Static %a" pp_elt x
    | Dynamic f -> Fmt.pf ppf "Dynamic %a" pp_elt (f ())
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
