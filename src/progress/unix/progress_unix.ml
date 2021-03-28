open Progress

let stopwatch () =
  Segment.stateful (fun () ->
      let start_time = Mtime_clock.counter () in
      Units.seconds (fun ~width pp_time ->
          let pp ppf = pp_time ppf (Mtime_clock.count start_time) in
          Segment.const_fmt ~width pp))

let counter (type elt) ~(total : elt) ?style ?message
    ?(pp : (elt, elt Segment.t) Units.pp_fixed option) ?width ?sampling_interval
    (module Elt : Progress.Elt with type t = elt) : (elt reporter -> 'a, 'a) t =
  Internal.counter ~prebar:(stopwatch ()) ~total ?style ?message ?pp ?width
    ?sampling_interval
    (module Elt)

let stderr_if_tty =
  if Unix.(isatty stderr) then Progress.Config.Default.ppf
  else Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

let default_config = Config.create ~ppf:stderr_if_tty ()

let with_reporters ?(config = Config.create ()) x =
  with_reporters ~config:Config.(config || default_config) x

let start ?(config = Config.create ()) x =
  start ~config:Config.(config || default_config) x

let finalize = finalize
let ( / ) = ( / )
