module L = Progress.Line

let bar ~total =
  let total_bytes = Fmt.to_to_string Progress.Units.Bytes.of_int total in
  let open L in
  let rate = Progress.Units.Bytes.(rate ~width of_float) (module Int) in
  let eta = eta ~total (module Int) in
  list ~sep:(const " ")
    [ spinner ~color:`Green ()
    ; const "[" ++ elapsed () ++ const "]"
    ; bar ~color:`Cyan ~style:`ASCII ~total (module Int)
    ; bytes ++ const " / " ++ const total_bytes
    ; const "(" ++ rate ++ const ", " ++ eta ++ const ")"
    ]

let run () =
  let total = 231231231 in
  let bar = Progress.make (bar ~total) in
  Progress.with_reporters bar @@ fun report ->
  let step = 22321 in
  for _ = 1 to (total / step) + 1 do
    report step;
    Unix.sleepf 0.0012
  done
