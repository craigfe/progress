let ( >> ) f g x = g (f x)

module List = struct
  include List

  let rec intersperse ~sep = function
    | ([] | [ _ ]) as l -> l
    | h1 :: (_ :: _ as tl) -> h1 :: sep :: intersperse ~sep tl
end

module Staging : sig
  type 'a staged

  val stage : 'a -> 'a staged
  val unstage : 'a staged -> 'a
  val ( let$ ) : 'a staged -> ('a -> 'b) -> 'b staged
  val ( and$ ) : 'a staged -> 'b staged -> ('a * 'b) staged
end = struct
  type 'a staged = 'a

  let stage a = a
  let unstage a = a
  let ( let$ ) x f = f x
  let ( and$ ) a b = (a, b)
end
