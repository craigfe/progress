let[@ocamlformat "disable"] bar_styles =
  let open Progress.Line.Bar_style in
  let open Progress.Color in
  let brackets = ("[", "]") in
  let bars = ("│", "│") in
  [ ("ASCII"    , ascii |> with_color (ansi `cyan) |> with_empty_color (ansi `blue))
  ; ("arrow"    , v ~delims:brackets ~color:(ansi `red) [ "="; ">"; " " ])
  ; ("dots"     , v ~delims:brackets ~color:(ansi `magenta) [ "." ])
  ; ("digits"   , v ~delims:brackets [ "9"; "8"; "7"; "6"; "5"; "4"; "3"; "2"; "1"; "0"])
  ; ("UTF8"     , utf8 |> with_color (ansi `green))
  ; ("rough bar", v ~delims:bars ~color:(hex "#DC2F02") [ "█"; " " ])
  ; ("fine bar" , v ~delims:bars ~color:(hex "#E85D04") [ "█"; "▉"; "▊"; "▋"; "▌"; "▍"; "▎"; "▏"; " " ] )
  ; ("vertical" , v ~delims:bars ~color:(hex "#F48C06") [ "█"; "▇"; "▆"; "▅"; "▄"; "▃"; "▂"; "▁"; " " ] )
  ; ("blocky"   , v ~delims:bars ~color:(hex "#FAA307") [ "█"; "▛"; "▌"; "▖"; " " ])
  ; ("fade in"  , v ~delims:bars ~color:(hex "#FFBA08") [ "█"; "▓"; "▒"; "░"; " " ])
  ]

let layout =
  let pick_colour = Utils.colour_picker () in
  let open Progress.Line in
  let bars =
    ListLabels.map bar_styles ~f:(fun (name, style) ->
        lpad 17 (constf "%s : " name)
        ++ bar ~style:(`Custom style) ~color:(pick_colour ()) 1000)
  in
  Progress.Multi.(blank ++ lines bars ++ blank)

let run () =
  Progress.with_reporters ~config:(Progress.Config.v ~max_width:(Some 50) ())
    layout (fun reporters ->
      for _ = 0 to 1000 do
        List.iter (fun f -> f 1) reporters;
        Unix.sleepf 0.006
      done)
