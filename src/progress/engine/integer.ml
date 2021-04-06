module type S = sig
  type t

  val zero : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val to_string : t -> string
  val to_float : t -> float
  val of_float : float -> t
end

module Int : S with type t = int = Int
module Int32 : S with type t = int32 = Int32
module Int64 : S with type t = int64 = Int64

module Float : S with type t = float = struct
  include Float

  let to_float x = x
  let of_float x = x
end
