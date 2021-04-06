open! Import
open Progress_engine

module Platform = struct
  module Clock = Mtime_clock
  module Terminal_width = Terminal_width
end

module Renderer = struct
  include Renderer.Platform_dependent (Platform)
  include Renderer
end

module Line = struct
  include Line.Platform_dependent (Platform)
  include Line
end

let renderer_config : Line.render_config =
  { interval = None; max_width = Some 120 }

module Multi = struct
  type 'a reporter = 'a -> unit
  type ('a, 'b) t = ('a, 'b) Renderer.t

  let ( / ) top bottom = Renderer.Segment_list.append top bottom
  let v x = Renderer.make (Line.compile x ~config:renderer_config)

  let v_list xs =
    Renderer.make_list (List.map (Line.compile ~config:renderer_config) xs)
end

type display = Renderer.display

let start = Renderer.start
let tick = Renderer.tick
let finalize = Renderer.finalize
let interject_with = Renderer.interject_with
let with_reporters = Renderer.with_reporters
let with_reporter ?config b f = with_reporters ?config (Multi.v b) f

module Reporters = Renderer.Reporters

type bar_style = [ `ASCII | `UTF8 | `Custom of string list ]

let counter (type elt) ~(total : elt) ?color ?(style = `ASCII) ?message ?pp
    ?width:_ ?sampling_interval:(_ = 1)
    (module Integer : Integer.S with type t = elt) =
  let module Line = struct
    include Line
    include Line.Integer_dependent (Integer)
  end in
  let open Line in
  list
    [ Option.fold ~none:(noop ()) message ~some:const
    ; Option.fold ~none:(noop ()) pp ~some:Line.of_printer
    ; Line.elapsed ()
    ; bar ?color ~style ~total ()
    ; percentage_of total
    ]

module Config = struct
  include Config

  let stderr_if_tty =
    if Unix.(isatty stderr) then Default.ppf
    else Format.make_formatter (fun _ _ _ -> ()) (fun () -> ())

  let create ?(ppf = stderr_if_tty) ?hide_cursor ?persistent () : t =
    { ppf = Some ppf; hide_cursor; persistent }
end

module Ansi = Ansi
module Duration = Duration
module Printer = Printer
module Units = Units
