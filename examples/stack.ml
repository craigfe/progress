let () =
  let bar message =
    Progress_unix.counter ~mode:`ASCII ~total:1_000_000L ~message
      ~pp:Progress.Units.bytes ()
  in
  Progress_unix.(
    with_reporters
      ( bar "index.html     "
      / bar "sitemap.xml    "
      / bar "img/kittens.jpg"
      / bar "img/puppies.jpg" ))
  @@ fun (((a, b), c), d) ->
  let pick_random () =
    match Random.int 100 with
    | n when n < 19 -> a
    | n when n < 58 -> b
    | n when n < 74 -> c
    | _ -> d
  in
  let random_progress () = Random.int64 10_000L in
  for _ = 1 to 1_250 do
    (pick_random ()) (random_progress ());
    Unix.sleepf 0.01
  done
