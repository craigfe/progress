let bars =
  [ ("Rough bar:  ", `Custom [ " "; "█" ], `Red)
  ; ( "Fine bar:   "
    , `Custom [ " "; "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉"; "█" ]
    , `Yellow )
  ; ( "Vertical:   "
    , `Custom [ " "; "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" ]
    , `Green )
  ; ("Blocky:     ", `Custom [ " "; "▖"; "▌"; "▛"; "█" ], `Magenta)
  ; ("Fade in:    ", `Custom [ " "; "░"; "▒"; "▓"; "█" ], `Blue)
  ]
  |> List.map (fun (label, style, color) ->
         let open Progress.Line in
         const label
         ++ bar ~style ~color ~total:1000 (module Int)
         ++ percentage_of 100 (module Int))
  |> Progress.make_list

let pick_random_function l =
  Random.self_init ();
  let len = List.length l in
  fun x -> List.nth l (Random.int len) x

let run () =
  Progress.with_reporters bars (fun reporters ->
      let random_reporter = pick_random_function reporters in
      for _ = 0 to 6000 do
        random_reporter 1;
        Unix.sleepf 0.002
      done)
