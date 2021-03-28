open! Import

module type Elt = sig
  type t

  val zero : t
  val add : t -> t -> t
  val to_float : t -> float
end

type 'a reporter = 'a -> unit
type display = Renderer.display
type ('a, 'b) t = ('a, 'b) Renderer.t

let make = Renderer.make
let make_list = Renderer.make_list
let start = Renderer.start
let finalize = Renderer.finalize
let interject_with = Renderer.interject_with
let with_reporters = Renderer.with_reporters
let ( / ) top bottom = Renderer.Segment_list.append top bottom

module Reporters = Renderer.Reporters

type bar_style = [ `ASCII | `UTF8 | `Custom of string list ]

module Internal = struct
  let counter (type elt) ?prebar ~(total : elt) ?color ?(style = `ASCII)
      ?message ?pp ?width ?(sampling_interval = 1)
      (module Elt : Elt with type t = elt) =
    let open Line in
    let proportion =
      let total = Elt.to_float total in
      fun i -> Elt.to_float i /. total
    in
    list
      (Option.fold ~none:[] message ~some:(fun s -> [ const s ])
      @ Option.fold ~none:[] pp ~some:(fun (pp, width) ->
            [ Segment.of_pp ~width pp ])
      @ Option.fold ~none:[] prebar ~some:(fun s -> [ s ])
      @ [ bar ?color ~style proportion
          ++ const " "
          ++ using proportion percentage
        ])
    |> Option.fold width ~some:Segment.box_fixed ~none:(fun s ->
           Segment.box_winsize ~fallback:80 s)
    |> Segment.periodic sampling_interval
    |> Segment.accumulator Elt.add Elt.zero
    |> make ~init:Elt.zero
end

let counter (type elt) ~total ?color ?style ?message ?pp ?width
    ?sampling_interval (module Elt : Elt with type t = elt) =
  Internal.counter ?color ?prebar:None ~total ?style ?message ?pp ?width
    ?sampling_interval
    (module Elt)

module Ansi = Ansi
module Config = Config
module Duration = Duration
module Line = Line
module Segment = Segment
module Units = Units
