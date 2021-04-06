(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

module Platform = struct
  module Clock = Mtime_clock
  module Terminal_width = Terminal_width
end

include Progress_engine.Make (Platform)

module Config = struct
  include Config

  let stderr_if_tty =
    if Unix.(isatty stderr) then Default.ppf
    else Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

  let create ?(ppf = stderr_if_tty) ?hide_cursor ?persistent () : t =
    { ppf = Some ppf; hide_cursor; persistent }
end
