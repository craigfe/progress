open Utils

let trimmed pp_fixed ppf =
  Format.asprintf "%a" (fst pp_fixed)
  >> String.trim
  >> Format.pp_print_string ppf

module Percentage = struct
  let clamp (lower, upper) = Float.min upper >> Float.max lower

  let pp_fixed =
    let pp ppf proportion =
      let percentage = clamp (0., 100.) (Float.trunc (proportion *. 100.)) in
      Format.fprintf ppf "%3.0f%%" percentage
    in
    (pp, 4)

  let pp = trimmed pp_fixed
end

module Bytes = struct
  let rec power = function 1 -> 1024L | n -> Int64.mul 1024L (power (n - 1))
  let conv exp = Int64.(of_int >> mul (power exp))
  let kib = conv 1
  let mib = conv 2
  let gib = conv 3
  let tib = conv 4
  let pib = conv 5

  (** Pretty-printer for byte counts *)
  let pp_fixed =
    (* Round down to the nearest 0.1 *)
    let tr f = Float.trunc (f *. 10.) /. 10. in
    let num = format_of_string "%6.1f " in
    let fpr = Format.fprintf in
    let pp ppf =
      Int64.to_float >> function
      | n when n < 1024. -> fpr ppf (num ^^ "B  ") (tr n)
      | n when n < 1024. ** 2. -> fpr ppf (num ^^ "KiB") (tr (n /. 1024.))
      | n when n < 1024. ** 3. ->
          fpr ppf (num ^^ "MiB") (tr (n /. (1024. ** 2.)))
      | n when n < 1024. ** 4. ->
          fpr ppf (num ^^ "GiB") (tr (n /. (1024. ** 3.)))
      | n when n < 1024. ** 5. ->
          fpr ppf (num ^^ "TiB") (tr (n /. (1024. ** 4.)))
      | n when n < 1024. ** 6. ->
          fpr ppf (num ^^ "PiB") (tr (n /. (1024. ** 5.)))
      | n -> fpr ppf (num ^^ "EiB") (tr (n /. (1024. ** 6.)))
    in
    (pp, 10)

  let pp = trimmed pp_fixed
end

type ('a, 'b) pp_fixed =
  (width:int -> (Format.formatter -> 'a -> unit) -> 'b) -> 'b

let percentage f = f ~width:(snd Percentage.pp_fixed) (fst Percentage.pp_fixed)
let bytes f = f ~width:(snd Bytes.pp_fixed) (fst Bytes.pp_fixed)

let seconds f =
  let pp ppf span =
    let seconds = Mtime.Span.to_s span in
    Format.fprintf ppf "%02.0f:%02.0f" (Float.div seconds 60.)
      (Float.rem seconds 60.)
  in
  f ~width:5 pp
