let () =
  let bar message =
    Progress.counter ~total:1_000_000L ~sampling_interval:1 ~message
      ~pp:Progress.bytes ()
  in
  Progress.(
    with_display
      ( bar "index.html     "
      <-> bar "sitemap.xml    "
      <-> bar "img/kittens.jpg"
      <-> bar "img/puppies.jpg" )) (fun (((a, b), c), d) ->
      let pick_random () =
        match Random.int 4 with
        | 0 -> a
        | 1 -> b
        | 2 -> c
        | 3 -> d
        | _ -> assert false
      in
      let random_progress () = Random.int64 10_000L in
      for _ = 1 to 1000 do
        (pick_random ()) (random_progress ());
        Unix.sleepf 0.1
      done)
