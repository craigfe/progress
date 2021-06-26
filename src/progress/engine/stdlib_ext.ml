(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

type 'a pp = Format.formatter -> 'a -> unit

let ( >> ) f g x = g (f x)

let tap f x =
  f x;
  x

let trace fmt x =
  Fmt.epr fmt x;
  x

module type Eq = sig
  type t

  val equal : t -> t -> bool
end

module type Comparable_infix = sig
  type t

  val ( = ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( >= ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( > ) : t -> t -> bool
end

module Poly = struct
  let ( = ) = Stdlib.( = )
  let ( <= ) = Stdlib.( <= )
  let ( >= ) = Stdlib.( >= )
  let ( < ) = Stdlib.( < )
  let ( > ) = Stdlib.( > )
end

include Stdlib.StdLabels
include Stdlib.MoreLabels

(** Shadow polymorphic operators in the Stdlib. *)
include struct
  let min : int -> int -> int = min
  let max : int -> int -> int = max
  let compare : int -> int -> int = compare

  include (Poly : Comparable_infix with type t := int)
end

module Int = struct
  include Int
  include (Poly : Comparable_infix with type t := t)

  let float_div a b = to_float a /. to_float b
end

module Int32 = struct
  include Int32
  include (Poly : Comparable_infix with type t := t)

  let float_div a b = to_float a /. to_float b
end

module Int63 = struct
  include Optint.Int63
  include (Poly : Comparable_infix with type t := t)

  let float_div a b = to_float a /. to_float b
end

type int63 = Int63.t

module Int64 = struct
  include Int64
  include (Poly : Comparable_infix with type t := t)

  let float_div a b = to_float a /. to_float b
end

module Float = struct
  include Float
  include (Poly : Comparable_infix with type t := t)

  let float_div = ( /. )
  let to_float x = x
  let of_float x = x
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
  type ('a, 'b) endo := 'a t -> 'b t

  external map : f:('a -> 'b) -> ('a, 'b) endo = "%identity"
  external inj : 'a -> 'a t = "%identity"
  external prj : 'a t -> 'a = "%identity"

  module Syntax : sig
    val ( let$ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and$ ) : 'a t -> 'b t -> ('a * 'b) t
  end
end = struct
  type 'a t = 'a
  type ('a, 'b) endo = 'a t -> 'b t

  external map : f:('a -> 'b) -> ('a, 'b) endo = "%identity"
  external inj : 'a -> 'a t = "%identity"
  external prj : 'a t -> 'a = "%identity"

  module Syntax = struct
    let ( let$ ) x f = f x
    let ( and$ ) a b = (a, b)
  end
end

module Unique_id () : sig
  type t

  val create : unit -> t
  val equal : t -> t -> bool
  val pp : t pp
end = struct
  let allocated = ref 0

  type t = int

  let create () =
    let v = !allocated in
    incr allocated;
    v

  let equal = Int.equal
  let pp = Fmt.int
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
