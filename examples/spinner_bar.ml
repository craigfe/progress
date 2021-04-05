open Progress

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

let bar =
  let spin = Line.spinner ~stages () in
  make Line.(const "[" ++ spin ++ spin ++ spin ++ spin ++ spin ++ const "]")

let run () =
  with_reporters bar (fun report ->
      for _ = 1 to 40 do
        report ();
        Unix.sleepf 0.1
      done)
