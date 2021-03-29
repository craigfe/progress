module L = Progress.Line

let bar ~total =
  let total_bytes = Fmt.to_to_string Progress.Units.Bytes.of_int total in
  let rate =
    L.using Int64.of_int
      (Progress_unix.rate Progress.Units.Bytes.(of_int64, width))
  in
  let eta = L.using Int64.of_int (Progress_unix.eta (Int64.of_int total)) in
  let open L in
  Progress_unix.debounced_accumulator (Progress.Duration.of_ms 0.0001) ( + ) 0
  @@ Expert.box_winsize ~fallback:80
  @@ list ~sep:(const " ")
       [ spinner ~color:`Green ()
       ; const "[" ++ Progress_unix.elapsed () ++ const "]"
       ; using Progress_unix.acc
         @@ bar ~color:`Cyan ~style:`ASCII (fun x ->
                float_of_int x /. float_of_int total)
       ; using Progress_unix.acc @@ (bytes ++ const " / " ++ const total_bytes)
       ; using Progress_unix.latest
         @@ (const "(" ++ rate ++ const ", " ++ eta ++ const ")")
       ]

let run () =
  let total = 231231231 in
  let bar = Progress.make ~init:0 (bar ~total) in
  Progress.with_reporters bar @@ fun report ->
  let step = 22321 in
  for _ = 1 to (total / step) + 1 do
    report step;
    Unix.sleepf 0.0012
  done
