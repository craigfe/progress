let () =
  let bar message =
    Progress.counter ~mode:`UTF ~total:1_000_000L ~message ~pp:Progress.bytes ()
  in
  Progress.(
    with_display
      ( bar "index.html     "
      / bar "sitemap.xml    "
      / bar "img/kittens.jpg"
      / bar "img/puppies.jpg" ))
  @@ fun (((a, b), c), d) ->
  let pick_random () =
    match Random.int 100 with
    | n when n < 19 -> a
    | n when n < 58 -> b
    | n when n < 70 -> c
    | _ -> d
  in
  let random_progress () = Random.int64 1_000L in
  for _ = 1 to 10_000 do
    (pick_random ()) (random_progress ());
    Unix.sleepf 0.001
  done
