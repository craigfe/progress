let bar ~total =
  let open Progress.Line in
  list
    [ spinner ~color:(Progress.Color.ansi `green) ()
    ; bar total
    ; count_to total
    ]

let run () =
  let total = 100 in
  Progress.with_reporter (bar ~total) (fun f ->
      for i = 1 to total do
        f 1;
        if i mod 10 = 0 then
          Progress.interject_with (fun () ->
              print_endline (":: Finished " ^ string_of_int i));
        Unix.sleepf 0.025
      done)
