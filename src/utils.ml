let ( >> ) f g x = g (f x)

module List = struct
  include List

  let rec interleave ~sep = function
    | ([] | [ _ ]) as l -> l
    | h1 :: (_ :: _ as tl) -> h1 :: sep :: interleave ~sep tl
end
