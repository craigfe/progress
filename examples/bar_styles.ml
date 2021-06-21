let bar_specs =
  [ ("Default ASCII", `ASCII, "#DC2F02")
  ; ("Rough bar", `Custom (" ", [ ">" ], "="), "#DC2F02")
  ; ("Rough bar", `UTF8, "#DC2F02")
  ; ("Default UTF-8", `Custom (" ", [], "█"), "#DC2F02")
  ; ( "Fine bar"
    , `Custom (" ", [ "▏"; "▎"; "▍"; "▌"; "▋"; "▊"; "▉" ], "█")
    , "#E85D04" )
  ; ( "Vertical"
    , `Custom (" ", [ "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇" ], "█")
    , "#F48C06" )
  ; ("Blocky", `Custom (" ", [ "▖"; "▌"; "▛" ], "█"), "#FAA307")
  ; ("Fade in", `Custom (" ", [ "░"; "▒"; "▓" ], "█"), "#FFBA08")
  ]

let bars =
  let pick_colour = Utils.colour_picker () in
  let open Progress.Line in
  ListLabels.map bar_specs ~f:(fun (name, style, _color) ->
      lpad 20 (constf "%s –  " name)
      ++ bar ~style ~color:(pick_colour ()) ~total:1000 ())
  |> Progress.Multi.lines

let pick_random_function l =
  Random.self_init ();
  let len = List.length l in
  fun x -> List.nth l (Random.int len) x

let run () =
  print_endline "";
  Progress.with_reporters ~config:(Progress.Config.v ~max_width:(Some 80) ())
    bars (fun reporters ->
      let random_reporter = pick_random_function reporters in
      for _ = 0 to 1000 * List.length bar_specs do
        random_reporter 1;
        Unix.sleepf 0.001
      done)
