let bar color message =
  Progress_unix.counter ~color ~style:`UTF8 ~total:1_000_000_000L ~message
    ~pp:Progress.Units.Bytes.(of_int64, width)
    (module Int64)

let main () =
  Progress_unix.(
    with_reporters
      (bar `Red "index.html     "
      / bar `Yellow "sitemap.xml    "
      / bar `Green "img/kittens.jpg"
      / bar `Blue "img/puppies.jpg"))
  @@ fun a b c d ->
  let pick_random () =
    match Random.int 100 with
    | n when n < 19 -> a
    | n when n < 58 -> b
    | n when n < 74 -> c
    | _ -> d
  in
  let random_progress () = Random.int64 10_000L in
  for i = 1 to 1_250_000 do
    if i mod 100 = 0 then Logs.info (fun f -> f "Iterations reached: %d" i);
    (pick_random ()) (random_progress ()) (* Unix.sleepf 0.01 *)
  done

let run () =
  Memtrace.trace_if_requested ();
  let () =
    (* Run with [dune exec examples/stack.exe -- --verbose] to see log entries
       being interleaved with progress bar rendering. *)
    match Sys.argv with
    | [| _ |] -> ()
    | [| _; "--verbose" |] ->
        (* Configure a [Logs] reporter that behaves properly with concurrent
           progress bar rendering. *)
        Logs.set_reporter
          (Progress_logs.instrument_reporter (Logs_fmt.reporter ()));
        Logs.set_level (Some Info)
    | _ ->
        Format.eprintf "usage: %s [--verbose]@." Sys.argv.(0);
        exit 1
  in
  main ()
