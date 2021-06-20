let bar =
  Progress.Line.(
    list
      [ spinner ~color:(Progress.Color.of_ansi `green) ()
      ; bar ~style:`ASCII ~total:100 ()
      ; count_up_to 100
      ])

let run () =
  Progress.with_reporter bar (fun f ->
      for i = 1 to 100 do
        f 1;
        if i mod 10 = 0 then
          Progress.interject_with (fun () ->
              print_endline ("[*] finished " ^ string_of_int i));
        Unix.sleepf 0.025
      done)
