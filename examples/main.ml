let examples =
  Examples.
    [ ("Bar_styles", Bar_styles.run)
    ; ("Download", Download.run)
    ; ("Installer", Installer.run)
    ; ("Interject", Interject.run)
    ; ("Spinner_bar", Spinner_bar.run)
    ; ("Spinner_long", Spinner_long.run)
    ; ("Stack", Stack.run)
    ]

let available_examples () =
  Format.eprintf "Available examples: @.";
  ListLabels.iter examples ~f:(fun (name, _) -> Format.eprintf "- %s@." name)

let usage () =
  Format.eprintf "@.";
  available_examples ();
  Format.eprintf "\n%a: dune exec %s%s%s.exe -- [--help] <example_name>@."
    Fmt.(styled `Green string)
    "usage" Filename.current_dir_name Filename.dir_sep
    (Filename.chop_extension __FILE__)

let () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  match Sys.argv with
  | [| _ |] | [| _; "-h" | "-help" | "--help" |] -> usage ()
  | [| _; name |] -> (
      match List.assoc_opt (String.capitalize_ascii name) examples with
      | None ->
          Format.eprintf "%a: unrecognised example name `%a`.@.@."
            Fmt.(styled `Bold @@ styled `Red string)
            "Error"
            Fmt.(styled `Cyan string)
            name;
          available_examples ();
          exit 1
      | Some f -> f ())
  | _ ->
      usage ();
      exit 1
