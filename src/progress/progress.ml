open! Import
open Progress_engine

module type Elt = sig
  type t

  val zero : t
  val add : t -> t -> t
  val to_float : t -> float
end

type 'a reporter = 'a -> unit

module Platform = struct
  module Width = Width
end

module Renderer = struct
  include Renderer
  include Make (Platform)
end

module Segment = struct
  include Segment
  include Platform_dependent (Platform)
end

module Line = struct
  include Line.Time_sensitive (Mtime_clock)
  include Line

  module Expert = struct
    include Expert.Platform_dependent (Platform)
    include Expert
  end
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

let counter (type elt) ~(total : elt) ?color ?(style = `ASCII) ?message ?pp
    ?width ?(sampling_interval = 1) (module Elt : Elt with type t = elt) =
  let open Line in
  let proportion =
    let total = Elt.to_float total in
    fun i -> Elt.to_float i /. total
  in
  list
    (Option.fold ~none:[] message ~some:(fun s -> [ const s ])
    @ Option.fold ~none:[] pp ~some:(fun (pp, width) ->
          [ Segment.of_pp ~width pp ])
    @ [ Line.elapsed () ]
    @ [ bar ?color ~style proportion ++ const " " ++ using proportion percentage
      ])
  |> Option.fold width ~some:Segment.box_fixed ~none:(fun s ->
         Segment.box_winsize ~fallback:80 s)
  |> Segment.periodic sampling_interval
  |> Segment.accumulator Elt.add Elt.zero
  |> make ~init:Elt.zero

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
module Units = Units
