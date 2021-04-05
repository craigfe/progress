let bar =
  Progress.make
    Progress.Line.(
      bar ~style:`UTF8 ~total:100 (module Int)
      ++ const " "
      ++ count 100 (module Int)
      ++ const "/100")

let run () =
  Progress.with_reporters bar (fun f ->
      for i = 1 to 100 do
        f 1;
        Progress.interject_with (fun () ->
            print_endline ("[*] finished " ^ string_of_int i));
        Unix.sleepf 0.025
      done)
