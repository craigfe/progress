open Progress

let bar color message =
  let total = 1_000_000_000L in
  let open Line.Using_int64 in
  list
    [ rpad 16 (constf " %s" message)
    ; bytes
    ; bytes_per_sec
    ; bar ~color ~style:`UTF8 ~total ()
    ; percentage_of total ++ const " "
    ]
  |> Multi.line

let main () =
  let layout =
    let open Multi in
    bar (Color.of_hex "#90e0ef") "index.html"
    ++ bar (Color.of_hex "#48cae4") "sitemap.xml"
    ++ bar (Color.of_hex "#00b4d8") "img/kittens.jpg"
    ++ bar (Color.of_hex "#0096c7") "img/puppies.jpg"
  in
  with_reporters layout @@ fun a b c d ->
  let pick_random () =
    match Random.int 100 with
    | n when n < 19 -> a
    | n when n < 58 -> b
    | n when n < 74 -> c
    | _ -> d
  in
  let random_progress () = Random.int64 1_000_000L in
  for i = 1 to 13_000 do
    if i mod 100 = 0 then Logs.info (fun f -> f "Iterations reached: %d" i);
    (pick_random ()) (random_progress ());
    Unix.sleepf 0.001
  done

let run () =
  let () =
    (* Run with [VERBOSE=true] to see log entries being interleaved with
       progress bar rendering. *)
    match Sys.getenv_opt "VERBOSE" with
    | None | Some "false" -> ()
    | Some _ ->
        (* Configure a [Logs] reporter that behaves properly with concurrent
           progress bar rendering. *)
        Logs.set_reporter
          (Progress_logs.instrument_reporter (Logs_fmt.reporter ()));
        Logs.set_level (Some Info)
  in
  main ()
