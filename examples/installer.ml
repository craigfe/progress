let line_prefix ~stages =
  let count = ref 0 in
  fun ppf ->
    incr count;
    Fmt.(styled `Bold (styled `Faint string))
      ppf
      (Fmt.str "[%d/%d]" !count stages)

let config = Progress.Config.create ~persistent:false ()

let with_plain_bar ~total f =
  Progress.(with_reporter ~config @@ counter ~total (module Int)) f

let with_bars f =
  let bar_names = [ "alcotest"; "ctypes"; "irmin"; "fmt"; "logs" ] in
  let bars =
    ListLabels.map bar_names ~f:(fun name ->
        let open Progress.Line in
        spinner ~color:(Progress.Ansi.Color.of_ansi `Green) ()
        ++ constf " %s: " name
        ++ string)
    |> Progress.Multi.v_list
  in
  Progress.(with_reporters ~config @@ bars) f

let pick_random l =
  let len = List.length l in
  fun () -> List.nth l (Random.int len)

let random_action =
  pick_random
    [ "cmake ."
    ; "make"
    ; "make clean"
    ; "gcc foo.c -o foo"
    ; "gcc bar.c -o bar"
    ; "./helper.sh rebuild-cache"
    ; "make all-clean"
    ; "make test"
    ]

let run () =
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
  let line_prefix = line_prefix ~stages:4 in
  let deps = 1234 in
  let started = Mtime_clock.counter () in
  Fmt.pr "%t 🔍  Resolving packages ... @." line_prefix;
  Fmt.pr "%t 🚚  Fetching packages ... @." line_prefix;
  Fmt.pr "%t 🔗  Linking %d dependencies ... @." line_prefix deps;
  with_plain_bar ~total:deps (fun f ->
      for _ = 1 to deps do
        f 1;
        Unix.sleepf 0.001
      done);
  Fmt.pr "%t 📃  Building fresh packages ... @." line_prefix;
  with_bars (fun reporters ->
      (* Give everyone something to do *)
      List.iter (fun f -> f (random_action ())) reporters;

      (* Finish a task every so often *)
      let random_reporter = pick_random reporters in
      for _ = 1 to 20 do
        random_reporter () (random_action ());

        (* Advance the spinners while we wait *)
        for _ = 1 to 10 do
          Progress.tick ();
          Unix.sleepf 0.05
        done
      done);
  Fmt.pr "✨ Done in %a@." Mtime.Span.pp (Mtime_clock.count started)
