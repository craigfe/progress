(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Progress_engine_intf

module Make (Platform : Platform.S) = struct
  module Ansi = Ansi
  module Duration = Duration
  module Printer = Printer
  module Units = Units
  module Config = Config

  module Renderer = struct
    include Renderer.Make (Platform)
    include Renderer
  end

  module Line = struct
    include Line.Platform_dependent (Platform)
    include Line
  end

  let renderer_config : Line.render_config =
    { interval = None; max_width = Some 120 }

  type 'a reporter = 'a -> unit

  module Multi = struct
    type ('a, 'b) t = ('a, 'b) Renderer.Segment_list.t

    let ( / ) top bottom = Renderer.Segment_list.append top bottom

    let v x =
      Renderer.Segment_list.One
        { segment = Line.compile x ~config:renderer_config }

    let v_list xs =
      Renderer.Segment_list.Many
        (List.map
           (fun x ->
             { Renderer.Segment_list.segment =
                 Line.compile ~config:renderer_config x
             })
           xs)
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
end

module Integer = Integer
