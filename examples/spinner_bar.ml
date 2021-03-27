let stages =
  let width = 6 in
  List.init width (fun i ->
      String.init width (fun x -> if x = i then '>' else '-'))

let spin = Progress.Segment.spinner ~stages ()

let bar =
  Progress.make ~init:()
    Progress.Segment.(
      const "[" ++ spin ++ spin ++ spin ++ spin ++ spin ++ const "]")

let () =
  Progress.with_reporters bar (fun report ->
      for _ = 1 to 40 do
        report ();
        Unix.sleepf 0.1
      done)
