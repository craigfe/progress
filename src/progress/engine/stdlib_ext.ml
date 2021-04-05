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
