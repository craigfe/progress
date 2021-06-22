let packages =
  [ ("0install-solver", "2.17")
  ; ("afl-persistent", "1.3")
  ; ("alcotest", "1.4.0")
  ; ("astring", "0.8.5")
  ; ("base", "v0.14.0")
  ; ("bechamel", "0.1.0")
  ; ("bos", "0.2.0")
  ; ("cmdliner", "1.0.4")
  ; ("cohttp", "4.0.0")
  ; ("core", "v0.14.1")
  ; ("ctypes", "0.17.1")
  ; ("dune", "2.8.4")
  ; ("either", "1.0.0")
  ; ("fmt", "0.8.9")
  ; ("fpath", "0.7.2")
  ; ("logs", "0.7.0")
  ; ("lru", "0.3.0")
  ; ("lwt", "5.4.0")
  ; ("memtrace", "0.1.2")
  ; ("mirage", "3.10.1")
  ; ("mirage-clock", "3.1.0")
  ; ("mirage-clock-unix", "3.1.0")
  ; ("mirage-crypto", "0.10.1")
  ; ("optint", "0.1.0")
  ; ("ppx_repr", "0.3.0")
  ; ("repr", "0.3.0")
  ; ("stdio", "v0.14.0")
  ; ("uucp", "13.0.0")
  ; ("uutf", "1.0.2")
  ; ("yojson", "1.7.0")
  ; ("zarith", "1.9.1")
  ]
  |> Vector.of_list ~dummy:("", "")

let setup_logs () =
  let reporter = Progress_logs.instrument_reporter (Logs_fmt.reporter ()) in
  Fmt_tty.setup_std_outputs ();
  Logs_threaded.enable ();
  Logs.set_reporter reporter

let bar =
  let open Progress.Line in
  let total = Vector.length packages in
  list
    [ constf "    %a" Fmt.(styled `Cyan string) "Building"
    ; using fst
        (brackets
           (bar
              ~style:(`Custom (Bar_style.v [ "="; ">"; " " ]))
              ~width:(`Fixed 57) ~total ()))
    ; using fst (count_up_to total) (* TODO: provide ticker_up_to *)
    ; using snd string
    ]

let rec package_worker (active_packages, reporter) =
  match Vector.pop packages with
  | exception Vector.Empty -> ()
  | package, version ->
      active_packages := package :: !active_packages;
      Logs.app (fun f ->
          f "   %a %s %s" Fmt.(styled `Green string) "Compiling" package version);
      Unix.sleepf (Random.float 1.);
      active_packages := List.filter (( <> ) package) !active_packages;
      reporter ();
      package_worker (active_packages, reporter)

let run () =
  setup_logs ();
  Random.self_init ();
  let cpus = 4 in
  let run_duration = Mtime_clock.counter () in
  let active_packages = ref [] in
  Progress.with_reporter ~config:(Progress.Config.v ~persistent:false ()) bar
    (fun reporter ->
      let reporter () =
        let package_list =
          !active_packages |> List.sort String.compare |> String.concat ", "
        in
        reporter (1, package_list)
      in
      let threads =
        List.init cpus (fun _ ->
            Thread.create package_worker (active_packages, reporter))
      in
      List.iter Thread.join threads);
  Logs.app (fun f ->
      f "    %a in %a"
        Fmt.(styled `Green string)
        "Finished" Mtime.Span.pp
        (Mtime_clock.count run_duration))
