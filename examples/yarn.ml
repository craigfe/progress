(* This example ports the following example from Indicatif (Rust progress bar
   library) to use Progress instead:
   https://github.com/mitsuhiko/indicatif/blob/556db194b5ffeb4596275f1ce1d477f300bb4626/examples/yarnish.rs *)

let line_prefix ~stages =
  let count = ref 0 in
  fun ppf ->
    incr count;
    Fmt.(styled `Bold (styled `Faint string))
      ppf
      (Fmt.str "[%d/%d]" !count stages)

let config = Progress.Config.v ~persistent:false ()

let with_plain_bar ~total f =
  Progress.(
    with_reporter ~config
      Line.(list [ elapsed (); bar ~total (); percentage_of total ])
      f)

let with_bars f =
  let bar_names = [ "alcotest"; "ctypes"; "irmin"; "fmt"; "logs" ] in
  let bars =
    ListLabels.map bar_names ~f:(fun name ->
        let open Progress.Line in
        spinner ~color:(Progress.Color.of_ansi `green) ()
        ++ constf " %s: " name
        ++ string)
    |> Progress.Multi.lines
  in
  let display = Progress.Display.start ~config bars in
  let [ reporters ] = Progress.Display.reporters display in
  let a = f display reporters in
  Progress.Display.finalise display;
  a

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
  with_bars (fun display reporters ->
      (* Give everyone something to do *)
      List.iter (fun f -> f (random_action ())) reporters;

      (* Finish a task every so often *)
      let random_reporter = pick_random reporters in
      for _ = 1 to 100 do
        random_reporter () (random_action ());

        (* Advance the spinners while we wait *)
        for _ = 1 to 5 do
          Progress.Display.tick display;
          Unix.sleepf 0.05
        done
      done);
  Fmt.pr "✨ Done in %a@." Mtime.Span.pp (Mtime_clock.count started)
