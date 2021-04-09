(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Progress_engine_intf

module Make (Platform : Platform.S) = struct
  module Color = Ansi.Color
  module Ansi = Ansi.Style
  module Duration = Duration
  module Printer = Printer
  module Units = Units

  module Config = struct
    include Config

    type t = user_supplied
  end

  module Renderer = struct
    include Renderer.Make (Platform)
    include Renderer
  end

  module Line = struct
    include Line.Platform_dependent (Platform)
    include Line
  end

  type 'a reporter = 'a -> unit

  module Multi = struct
    type ('a, 'b) t = ('a, 'b) Renderer.Segment_list.t

    let ( / ) top bottom = Renderer.Segment_list.append top bottom
    let v x = Renderer.Segment_list.One (fun config -> Line.compile x config)

    let v_list xs =
      Renderer.Segment_list.Many
        (List.map (fun x config -> Line.compile x config) xs)
  end

  type display = Renderer.display

  let start = Renderer.start
  let tick = Renderer.tick
  let finalize = Renderer.finalize
  let interject_with = Renderer.interject_with
  let with_reporters = Renderer.with_reporters
  let with_reporter ?config b f = with_reporters ?config (Multi.v b) f
  let add_line = Renderer.add_line

  module Reporters = Renderer.Reporters
end

module Integer = Integer

(*————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>

   Permission to use, copy, modify, and/or distribute this software for
   any purpose with or without fee is hereby granted, provided that the
   above copyright notice and this permission notice appear in all
   copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
  ————————————————————————————————————————————————————————————————————————————*)
