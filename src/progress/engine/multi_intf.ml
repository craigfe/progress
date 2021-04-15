(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

type 'a reporter = 'a -> unit

module type S = sig
  type 'a reporter

  type ('a, 'b) t
  (** The type of sequences of progress bars. The parameter ['a] stores a list
      of the reporting functions associated with each bar, terminating with
      ['b]. For example:

      {[
        (* Single progress bar, taking a [float] value. *)
        (float reporter -> 'b, 'b) t

        (* A two-bar layout, where the top bar takes [int64]s and the bottom one
           takes [string * float] pairs. *)
        (int64 reporter -> (string * float) reporter -> 'b, 'b) t
      ]}

      These reporting functions are supplied when beginning the {{!rendering}
      rendering} process. *)

  val blank : ('a, 'a) t

  val v : 'a Line.t -> ('a reporter -> 'b, 'b) t
  (** Define a new progress bar from a specification, with the given initial
      value. *)

  val v_list : 'a Line.t list -> ('a reporter list -> 'b, 'b) t

  val ( ++ ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** Stack progress bars vertically. [a / b] is a set with [a] stacked on top
      of [b]. The two bars have separate reporting functions, passed
      consecutively to the {!with_reporters} continuation when rendering. *)
end

module Hlist (Elt : sig
  type 'a t
end) =
struct
  type (_, _) t =
    | Zero : ('a, 'a) t
    | One : 'a Elt.t -> ('a reporter -> 'b, 'b) t
    | Many : 'a Elt.t list -> ('a reporter list -> 'b, 'b) t
    | Plus : (('a, 'b) t * ('b, 'c) t) -> ('a, 'c) t

  type 'b mapper = { f : 'a. int -> 'a Elt.t option -> 'b }

  let mapi =
    let rec aux : type a b c. (a, b) t -> int -> f:c mapper -> int * c list =
     fun t i ~f ->
      match t with
      | Zero -> (succ i, [ f.f i None ])
      | One b -> (succ i, [ f.f i (Some b) ])
      | Many bs ->
          ( i + List.length bs
          , List.mapi bs ~f:(fun i' x -> f.f (i + i') (Some x)) )
      | Plus (xs, ys) ->
          let i, xs = aux xs ~f i in
          let i, ys = aux ys ~f i in
          (i, xs @ ys)
    in
    fun t ~f -> snd (aux t 0 ~f)

  let rec length : type a b. (a, b) t -> int = function
    | Zero -> 1
    | One _ -> 1
    | Many xs -> List.length xs
    | Plus (a, b) -> length a + length b

  let ( ++ ) xs ys = Plus (xs, ys)
end

module type Multi = sig
  module type S = S

  module Hlist = Hlist

  include
    S
      with type 'a reporter := 'a -> unit
       and type ('a, 'b) t = ('a, 'b) Hlist(Line).t
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
