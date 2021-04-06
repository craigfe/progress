open Progress

let unlimited_bar =
  let stages =
    let width = 6 in
    List.init width (fun i ->
        String.concat ""
          (List.init width (fun x ->
               if x = i then
                 Ansi.(Style.code @@ Style.fg @@ Color.of_ansi `Cyan)
                 ^ Ansi.Style.(code bold)
                 ^ ">"
                 ^ Ansi.Style.(code none)
               else Ansi.Style.(code faint) ^ "-" ^ Ansi.Style.(code none))))
  in
  let spin = Line.spinner ~stages () in
  Line.(const "[" ++ spin ++ spin ++ spin ++ spin ++ spin ++ const "]")

let arrows =
  let stages =
    [ "▹▹▹▹▹"
    ; "▸▹▹▹▹"
    ; "▹▸▹▹▹"
    ; "▹▹▸▹▹"
    ; "▹▹▹▸▹"
    ; "▹▹▹▹▸"
    ]
  in

  Line.(
    spinner ~color:(Progress.Ansi.Color.of_ansi `Blue) ~stages ()
    ++ const "  Calculating...")

let run () =
  print_endline "";
  with_reporters
    (Multi.v_list
       Line.
         [ lpad 20 (const "unlimited bar  :  ") ++ unlimited_bar
         ; lpad 20 (const "arrows  :  ") ++ arrows
         ])
    (fun reporters ->
      for _ = 1 to 40 do
        List.iter (fun f -> f ()) reporters;
        Unix.sleepf 0.1
      done)
