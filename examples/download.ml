open Progress

let bar ~total =
  let open Line in
  let rate = rate Units.Bytes.of_float in
  let eta = eta ~total in
  list ~sep:(const " ")
    [ debounce (Duration.of_ms 80.) (spinner ~color:(Color.of_ansi `Green) ())
    ; const "[" ++ elapsed () ++ const "]"
    ; bar ~color:(Color.of_ansi `Cyan) ~style:`ASCII ~total ()
    ; bytes ++ constf " / %a" (Printer.to_pp Units.Bytes.of_int) total
    ; const "(" ++ rate ++ const ", eta: " ++ eta ++ const ")"
    ]

let ( / ) = Stdlib.( / )

let run () =
  let total = 231231231 in
  with_reporter (bar ~total) @@ fun report ->
  let step = 22321 in
  for _ = 1 to (total / step) + 1 do
    report step;
    Unix.sleepf 0.0012
  done
