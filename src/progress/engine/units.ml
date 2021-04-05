(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

open! Import

module Percentage = struct
  let clamp (lower, upper) = Float.min upper >> Float.max lower

  let of_float ppf proportion =
    let percentage = clamp (0., 100.) (Float.trunc (proportion *. 100.)) in
    Format.fprintf ppf "%3.0f%%" percentage

  let width = 4
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
  let pp_fixed to_float =
    (* Round down to the nearest 0.1 *)
    let tr f = Float.trunc (f *. 10.) /. 10. in
    let num = format_of_string "%6.1f " in
    let fpr = Format.fprintf in
    fun ppf ->
      to_float >> function
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

  let width = 10
  let of_int = pp_fixed Int.to_float
  let of_int64 = pp_fixed Int64.to_float
  let of_float = pp_fixed Fun.id
end

module Duration = struct
  let mm_ss ppf span =
    let seconds = Mtime.Span.to_s span in
    Format.fprintf ppf "%02.0f:%02.0f"
      (Float.div seconds 60. |> Float.floor)
      (Float.rem seconds 60. |> Float.floor)

  let mm_ss_print =
    let to_string span =
      let seconds = Mtime.Span.to_s span in
      Printf.sprintf "%02.0f:%02.0f"
        (Float.div seconds 60. |> Float.floor)
        (Float.rem seconds 60. |> Float.floor)
    in
    Print.of_to_string ~len:5 to_string
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
