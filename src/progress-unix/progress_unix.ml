let stopwatch () =
  Progress.Segment.stateful (fun () ->
      let start_time = Mtime_clock.counter () in
      Progress.Units.seconds (fun ~width pp_time ->
          let pp ppf = pp_time ppf (Mtime_clock.count start_time) in
          Progress.Segment.const_fmt ~width pp))

let counter = Progress.Internal.counter ~prebar:(stopwatch ())

let stderr_if_tty =
  if Unix.(isatty stderr) then Format.err_formatter
  else Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let with_reporters x = Progress.with_reporters ~ppf:stderr_if_tty x
let start x = Progress.start ~ppf:stderr_if_tty x
let finalise = Progress.finalise
let ( / ) = Progress.( / )
