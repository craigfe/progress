let () =
  let bar =
    Progress.make ~init:0
      Progress.Line.(
        Expert.box_winsize
        @@ Expert.accumulator ( + ) 0
        @@ bar ~style:`UTF8 (fun x -> float_of_int x /. 100.))
  in
  Progress.with_reporters bar (fun f ->
      for i = 1 to 100 do
        f 1;
        Progress.interject_with (fun () ->
            print_endline ("[*] finished " ^ string_of_int i));
        Unix.sleepf 0.025
      done)
