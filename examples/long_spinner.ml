let () =
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty (* TODO: make consistent *);
  let stages =
    [
      "▹▹▹▹▹";
      "▸▹▹▹▹";
      "▹▸▹▹▹";
      "▹▹▸▹▹";
      "▹▹▹▸▹";
      "▹▹▹▹▸";
    ]
  in
  let bar =
    Progress.make ~init:()
      Progress.Segment.(
        spinner ~color:`Blue ~stages () ++ const "  Calculating...")
  in
  Progress.with_reporters bar (fun report ->
      for _ = 1 to 40 do
        report ();
        Unix.sleepf 0.1
      done);
  Fmt.pr "%a  Done@." Fmt.(styled `Blue string) "▪▪▪▪▪";
  Unix.sleepf 0.5
