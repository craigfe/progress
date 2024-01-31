open Progress

let pick_colour = Utils.colour_picker ()

let bar ~total =
  let open Line in
  let spinner = spinner ~color:(Color.ansi `green) () in
  let bar = bar ~color:(pick_colour ()) ~style:`ASCII total in
  list
    [ spinner
    ; brackets (elapsed ())
    ; bar
    ; bytes
    ; parens (const "eta: " ++ eta total)
    ]

(* Simple mock for a worker performing a download action. *)
module Worker = struct
  type t =
    { mutable todo : int
    ; mutable download_rate : int
    ; mutable reporter : int Reporter.t option
    }

  let empty () = { todo = 0; reporter = None; download_rate = 100_000 }

  let make_progress t =
    let progress = min t.todo t.download_rate in
    t.download_rate <- max 0 (t.download_rate + (Random.int 50_001 - 25_000));
    t.todo <- t.todo - progress;
    Reporter.report (Option.get t.reporter) progress
end

type t = { mutable active_workers : Worker.t list; mutable files : int list }

let run () =
  let remove_lines =
    (* Run with [REMOVE_LINES=true] to see bars being removed from the
       display after the download is done. *)
    match Sys.getenv_opt "REMOVE_LINES" with
    | None | Some "false" -> false
    | Some _ -> true
  in
  let total_files = 18 in
  let files = List.init total_files (fun _ -> Random.int 100_000_000) in

  let bottom_line =
    Line.(
      spacer 4
      ++ ticker_to ~sep:(const " / ") total_files
      ++ const " files downloaded, elapsed: "
      ++ elapsed ())
  in
  let display =
    Display.start Multi.(blank ++ blank ++ line bottom_line ++ blank)
  in
  let [ completed ] = Display.reporters display in
  let nb_workers = 5 in
  let finish_item (worker : Worker.t) =
    Reporter.finalise (Option.get worker.reporter);
    completed ();
    if remove_lines then
      Display.remove_line display (Option.get worker.reporter)
  in
  let pick_item t (worker : Worker.t) =
    match t.files with
    | [] -> false
    | x :: xs ->
        let new_reporter = Display.add_line ~above:3 display (bar ~total:x) in
        worker.reporter <- Some new_reporter;
        worker.todo <- x;
        t.files <- xs;
        true
  in
  let t =
    { files; active_workers = List.init nb_workers (fun _ -> Worker.empty ()) }
  in

  (* Give everyone something to do *)
  ListLabels.iter t.active_workers ~f:(fun x ->
      let item_available = pick_item t x in
      assert item_available);

  (* Keep going until all files are downloaded *)
  while List.length t.active_workers > 0 do
    let active_workers =
      ListLabels.filter t.active_workers ~f:(fun (worker : Worker.t) ->
          Worker.make_progress worker;
          if worker.todo > 0 then true (* Not done yet; keep going. *)
          else (
            finish_item worker;
            pick_item t worker))
    in

    t.active_workers <- active_workers;
    Unix.sleepf 0.01
  done;
  Display.finalise display
