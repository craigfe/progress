open Progress

let colors =
  (* import matplotlib.cm
     for i in matplotlib.cm.rainbow(numpy.linspace(0.2, 1, 20)):
       print(matplotlib.colors.rgb2hex(i))
  *)
  Array.map Color.of_hex
    [| "#1996f3"; "#06aeed"; "#10c6e6"; "#27dade"; "#3dead5"
     ; "#52f5cb"; "#66fcc2"; "#7dffb6"; "#92fda9"; "#a8f79c"
     ; "#bced8f"; "#d2de81"; "#e8cb72"; "#feb562"; "#ff9b52"
     ; "#ff8143"; "#ff6232"; "#ff4121"; "#ff1f10"; "#ff0000"
    |]
  [@@ocamlformat "disable"]

let x = ref 0

let bar ~total =
  let open Line in
  let eta = eta ~total in
  incr x;
  list ~sep:(const " ")
    [ spinner ~color:(Color.of_ansi `green) ()
    ; const "[" ++ elapsed () ++ const "]"
    ; bar ~color:colors.(!x - 1) ~style:`ASCII ~total ()
    ; bytes
    ; const "(" ++ const "eta: " ++ eta ++ const ")"
    ]

type worker = { mutable todo : int; mutable reporter : int Reporter.t }
type t = { mutable active_workers : worker list; mutable files : int list }

let run () =
  let total_files = 20 in
  let files = List.init total_files (fun _ -> Random.int 10_000_000) in

  let bottom_line =
    Line.(
      lpad 8 (counter ())
      ++ constf " / %d files downloaded, elapsed: " total_files
      ++ elapsed ())
  in
  let display =
    Display.start Multi.(blank ++ blank ++ line bottom_line ++ blank)
  in
  let Reporters.[ completed ] =
    (Display.reporters display : (_, unit) Reporters.t)
  in
  let nb_workers = 5 in
  let active_workers =
    List.init nb_workers (fun _ -> { todo = 0; reporter = Reporter.noop })
  in
  let finish_item reporter =
    Display.finalize_line display reporter;
    completed ()
  in
  let t = { active_workers; files } in

  while List.length t.active_workers > 0 do
    let active_workers =
      ListLabels.filter t.active_workers ~f:(fun worker ->
          let progress = min worker.todo (2_000 + Random.int 8_000) in
          worker.todo <- worker.todo - progress;
          Reporter.push worker.reporter progress;
          match (worker.todo = 0, t.files) with
          | false, _ -> true (* Not done yet; keep going. *)
          | true, x :: xs ->
              (* Done. Take a new work item. *)
              finish_item worker.reporter;
              let new_reporter =
                Display.add_line ~above:3 display (bar ~total:x)
              in
              worker.reporter <- new_reporter;
              worker.todo <- x;
              t.files <- xs;
              true
          | true, [] ->
              (* Done, and no more items to do. *)
              finish_item worker.reporter;
              false)
    in
    t.active_workers <- active_workers;
    Unix.sleepf 0.01
  done;
  Display.finalize display
