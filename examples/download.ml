module S = Progress.Segment

let () = Fmt.set_style_renderer Fmt.stderr `Ansi_tty

let bar ~total =
  let total_bytes =
    Fmt.to_to_string Progress.Units.Bytes.pp (Int64.of_int total)
  in
  let open S in
  box_winsize ~max:80 ~fallback:80
  @@ accumulator ( + ) 0
  @@ list ~sep:(const " ")
       [
         spinner ~color:`Green ();
         const "[" ++ Progress_unix.stopwatch () ++ const "]";
         bar ~color:`Cyan ~mode:`ASCII (fun x ->
             float_of_int x /. float_of_int total);
         bytes ++ const " / " ++ const total_bytes;
       ]

let () =
  let total = 231231231 in
  let bar = Progress.make ~init:0 (bar ~total) in
  Progress.with_reporters bar @@ fun report ->
  let step = 223211 in
  for _ = 1 to (total / step) + 1 do
    report 223211;
    Unix.sleepf 0.0012
  done
