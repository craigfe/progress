let bars =
  [ ("Rough bar:  ", `Custom [ " "; "█" ], `Red)
  ; ( "Fine bar:   "
    , `Custom [ " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" ]
    , `Yellow )
  ; ( "Vertical:   "
    , `Custom [ " "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" ]
    , `Green )
  ; ("Fade in:    ", `Custom [ " "; "░"; "▒"; "▓"; "█" ], `Blue)
  ; ("Blocky:     ", `Custom [ " "; "▖"; "▌"; "▛"; "█" ], `Magenta)
  ]
  |> List.map (fun (label, style, color) ->
         let open Progress.Line in
         Expert.box_winsize ~max:50
         @@ Expert.accumulator ( + ) 0
         @@ (const label ++ bar ~style ~color (fun x -> float_of_int x /. 1000.)))
  |> Progress.make_list ~init:0

let pick_random_function l =
  Random.self_init ();
  let len = List.length l in
  fun x -> List.nth l (Random.int len) x

let () =
  Progress.with_reporters bars (fun reporters ->
      let random_reporter = pick_random_function reporters in
      for _ = 0 to 6000 do
        random_reporter 1;
        Unix.sleepf 0.002
      done)
