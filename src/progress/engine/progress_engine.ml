(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Progress_engine_intf

module type Platform = Platform.S

module Make (Platform : Platform) = struct
  module Color = Ansi.Color
  module Duration = Duration
  module Multi = Multi
  module Printer = Printer
  module Units = Units

  module Internals = struct
    module Ansi = Ansi.Style
  end

  module Config = struct
    include Config

    type t = user_supplied
  end

  module Renderer = struct
    include Renderer.Make (Platform)
    include Renderer
  end

  module Line = struct
    include Line.Make (Platform)
    include Line
  end

  module Display = Renderer.Display
  module Reporter = Renderer.Reporter

  let counter ~total ?(style = `ASCII) ?message ?pp () =
    let map_option ~f x = Option.fold ~none:(Line.noop ()) ~some:f x in
    let open Line.Using_int64 in
    list
      [ map_option message ~f:const
      ; map_option pp ~f:of_printer
      ; elapsed ()
      ; bar ~style ~total ()
      ; percentage_of total
      ]

  let interject_with = Renderer.interject_with
  let with_reporters = Renderer.with_reporters
  let with_reporter ?config b f = with_reporters ?config (Multi.line b) f
end

module Integer = Integer

(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
  ————————————————————————————————————————————————————————————————————————————*)
