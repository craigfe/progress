open! Import

type 'a reporter = 'a -> unit

module Hlist = struct
  (* ['a] and ['b] correspond to parameters of [Bar_list.t]. *)
  type (_, _) t =
    | [] : ('a, 'a) t
    | ( :: ) : 'a * ('b, 'c) t -> ('a -> 'b, 'c) t

  let rec apply_all : type a b. a -> (a, b) t -> b =
   fun f -> function [] -> f | x :: xs -> apply_all (f x) xs

  let rec append : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
   fun xs ys -> match xs with [] -> ys | x :: xs -> x :: append xs ys
end

module Reporters = struct
  type _ t = [] : unit t | ( :: ) : 'a * 'b t -> ('a -> 'b) t

  let rec of_hlist : type a. (a, unit) Hlist.t -> a t = function
    | [] -> []
    | x :: xs -> x :: of_hlist xs
end

module Segment_list = struct
  type 'a elt = { segment : 'a Segment.t; init : 'a }

  type (_, _) t =
    | One : 'a elt -> ('a reporter -> 'b, 'b) t
    | Many : 'a elt list -> ('a reporter list -> 'b, 'b) t
    | Plus : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t

  let append : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
   fun x y -> Plus (x, y)
end

module type Renderer = sig
  module Hlist = Hlist
  module Reporters = Reporters
  module Segment_list = Segment_list

  type display
  type ('a, 'b) t = ('a, 'b) Segment_list.t

  val make : init:'a -> 'a Segment.t -> ('a reporter -> 'b, 'b) t
  val make_list : init:'a -> 'a Segment.t list -> ('a reporter list -> 'b, 'b) t
  val start : ?config:Config.t -> ('a, unit) t -> 'a Reporters.t * display
  val finalize : display -> unit
  val with_reporters : ?config:Config.t -> ('a, 'b) t -> 'a -> 'b
  val interject_with : (unit -> 'a) -> 'a
end
