module Line = Progress.Line

let stages =
  let width = 6 in
  List.init width (fun i ->
      String.init width (fun x -> if x = i then '>' else '-'))

let spin = Line.spinner ~stages ()

let bar =
  Progress.make ~init:()
    Line.(const "[" ++ spin ++ spin ++ spin ++ spin ++ spin ++ const "]")

let run () =
  Progress.with_reporters bar (fun report ->
      for _ = 1 to 40 do
        report ();
        Unix.sleepf 0.1
      done)
