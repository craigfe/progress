open Progress

let bar ~total =
  let open Line in
  let rate = rate Units.Bytes.of_float in
  let eta = eta ~total in
  list ~sep:(const " ")
    [ spinner ~color:(Color.of_ansi `green) ()
    ; const "[" ++ elapsed () ++ const "]"
    ; bar ~color:(Color.of_ansi `cyan) ~style:`ASCII ~total ()
    ; bytes ++ constf " / %s" (Printer.to_to_string Units.Bytes.of_int total)
    ; const "(" ++ rate ++ const ", eta: " ++ eta ++ const ")"
    ]

type worker = { mutable todo : int; mutable reporter : int -> unit }
type t = { mutable active_workers : worker list; mutable files : int list }

let run () =
  let _, display = start (Multi.v (Line.noop ())) in
  let nb_workers = 5 in
  let active_workers =
    List.init nb_workers (fun _ -> { todo = 0; reporter = ignore })
  in
  let files = List.init 50 (fun _ -> Random.int 10_000_000) in

  let t = { active_workers; files } in

  while List.length t.active_workers > 0 do
    t.active_workers <-
      ListLabels.filter t.active_workers ~f:(fun worker ->
          let progress = Random.int 10_000 in
          (* Printf.printf "%d - %d\n%!" worker.todo progress; *)
          worker.todo <- max 0 (worker.todo - progress);
          worker.reporter progress;
          match (worker.todo = 0, t.files) with
          | false, _ -> true (* Not done yet; keep going. *)
          | true, x :: xs ->
              (* Done. Take a new work item. *)
              let new_reporter = add_line display (bar ~total:x) in
              worker.reporter <- new_reporter;
              worker.todo <- x;
              t.files <- xs;
              true
          | true, [] ->
              (* Done, and no more items to do. *)
              false);
    Unix.sleepf 0.01
  done
