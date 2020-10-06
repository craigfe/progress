type 'a fmt = Format.formatter -> 'a -> unit
type bytes = int64

let ( >> ) f g x = g (f x)
let kib = Int64.(of_int >> mul 1024L)
let mib = kib >> Int64.mul 1024L
let gib = mib >> Int64.mul 1024L

(** Pretty-printer for byte counts *)
let pp : int64 fmt =
  (* Round down to the nearest 0.1 *)
  let trunc f = Float.trunc (f *. 10.) /. 10. in
  fun ppf ->
    Int64.to_float >> function
    | n when n < 1024. -> Format.fprintf ppf "%6.1f B  " (trunc n)
    | n when n < 1024. ** 2. ->
        Format.fprintf ppf "%6.1f KiB" (trunc (n /. 1024.))
    | n when n < 1024. ** 3. ->
        Format.fprintf ppf "%6.1f MiB" (trunc (n /. (1024. ** 2.)))
    | n -> Format.fprintf ppf "%6.1f GiB" (trunc (n /. (1024. ** 3.)))

let pp_fixed = (pp, 10)
