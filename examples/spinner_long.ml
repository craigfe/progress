let stages =
  [ "▹▹▹▹▹"
  ; "▸▹▹▹▹"
  ; "▹▸▹▹▹"
  ; "▹▹▸▹▹"
  ; "▹▹▹▸▹"
  ; "▹▹▹▹▸"
  ]

let bar =
  Progress.make ~init:()
    Progress.Line.(spinner ~color:`Blue ~stages () ++ const "  Calculating...")

let run () =
  Progress.with_reporters bar (fun report ->
      for _ = 1 to 40 do
        report ();
        Unix.sleepf 0.1
      done);
  Fmt.pr "%a  Done@." Fmt.(styled `Blue string) "▪▪▪▪▪";
  Unix.sleepf 0.5
