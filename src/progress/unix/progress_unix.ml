open Progress
include Line.Time_sensitive (Mtime_clock)

let counter (type elt) ~total ?color ?style ?message ?pp ?width
    ?sampling_interval (module Elt : Progress.Elt with type t = elt) :
    (elt reporter -> 'a, 'a) t =
  Internal.counter ~prebar:(elapsed ()) ~total ?color ?style ?message ?pp ?width
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
