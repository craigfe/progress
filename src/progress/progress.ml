open! Import
open Progress_engine

type 'a reporter = 'a -> unit

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

type ('a, 'b) t = ('a, 'b) Renderer.t
type display = Renderer.display

let make = Renderer.make
let make_list = Renderer.make_list
let start = Renderer.start
let tick = Renderer.tick
let finalize = Renderer.finalize
let interject_with = Renderer.interject_with
let with_reporters = Renderer.with_reporters
let ( / ) top bottom = Renderer.Segment_list.append top bottom

module Reporters = Renderer.Reporters

type bar_style = [ `ASCII | `UTF8 | `Custom of string list ]

let renderer_config : Line.render_config =
  { interval = None; max_width = Some 120 }

let make x =
  let seg = Line.compile x ~config:renderer_config in
  make seg

let make_list xs =
  let xs = List.map (Line.compile ~config:renderer_config) xs in
  make_list xs

let counter (type elt) ~(total : elt) ?color ?(style = `ASCII) ?message ?pp
    ?width:_ ?sampling_interval:(_ = 1)
    (module Integer : Integer.S with type t = elt) =
  let open Line in
  make
  @@ list
       [ Option.fold ~none:(noop ()) message ~some:const
       ; Option.fold ~none:(noop ()) pp
           ~some:(Line.of_printer ~elt:(module Integer))
       ; Line.elapsed ()
       ; (bar ?color ~style ~total (module Integer) : elt Line.t)
       ; percentage_of total (module Integer)
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
