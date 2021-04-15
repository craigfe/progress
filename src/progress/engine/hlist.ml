type 'a base = |

module Make (Elt : sig
  type 'a t
end) =
struct
  type (_, _) t =
    | [] : ('a, 'a) t
    | ( :: ) : ('a Elt.t * ('b, 'c) t) -> ('a Elt.t -> 'b, 'c) t

  let rec append : type a b c. (a, b) t -> (b, c) t -> (a, c) t =
   fun xs ys -> match xs with [] -> ys | x :: xs -> x :: append xs ys

  let rec apply_all : type a b. a -> (a, b) t -> b =
   fun f -> function [] -> f | x :: xs -> apply_all (f x) xs
end
