let line_prefix ~stages =
  let count = ref 0 in
  fun ppf ->
    incr count;
    Fmt.(styled `Bold (styled `Faint string))
      ppf
      (Fmt.str "[%d/%d]" !count stages)

let with_plain_bar ~total f =
  Progress.(
    with_reporters ~config:(Config.create ~persistent:false ())
    @@ counter ~total:(Int64.of_int total) ())
    f

let rec iter n f =
  match n with
  | 0 -> ()
  | n ->
      f ();
      iter (n - 1) f

let () =
  Fmt.set_style_renderer Fmt.stdout `Ansi_tty;
  let line_prefix = line_prefix ~stages:4 in
  let deps = 1234 in
  let started = Mtime_clock.counter () in
  Fmt.pr "%t 🔍  Resolving packages ... @." line_prefix;
  Fmt.pr "%t 🚚  Fetching packages ... @." line_prefix;
  Fmt.pr "%t 🔗  Linking %d dependencies ... @." line_prefix deps;
  with_plain_bar ~total:deps (fun f ->
      iter deps (fun () ->
          f 1L;
          Unix.sleepf 0.001));
  Fmt.pr "%t 📃  Building fresh packages ... @." line_prefix;
  Fmt.pr "✨ Done in %a@." Mtime.Span.pp (Mtime_clock.count started)
