external sigwinch : unit -> int = "ocaml_progress_sigwinch"

let on_change = ref (fun _ -> ())
let latest_width = ref None

let initialise =
  let handle_signal _ =
    let width = Terminal_size.get_columns () in
    latest_width := width;
    !on_change width
  in
  lazy
    (latest_width := Terminal_size.get_columns ();
     Sys.set_signal (sigwinch ()) (Signal_handle handle_signal))

let set_changed_callback f =
  Lazy.force initialise;
  on_change := f

let columns () =
  Lazy.force_val initialise;
  !latest_width
