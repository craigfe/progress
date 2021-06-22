let total = 100

let bar =
  Progress.Line.(
    list
      [ spinner ~color:(Progress.Color.ansi `green) ()
      ; bar ~style:`ASCII total
      ; count_up_to total
      ])

let run () =
  Progress.with_reporter bar (fun f ->
      for i = 1 to total do
        f 1;
        if i mod 10 = 0 then
          Progress.interject_with (fun () ->
              print_endline (":: Finished " ^ string_of_int i));
        Unix.sleepf 0.025
      done)
