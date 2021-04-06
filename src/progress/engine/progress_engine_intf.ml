(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

module type S = sig
  (** {2 Preliminary types and utilities} *)

  module Color : sig
    include module type of Ansi.Color
    (** @inline *)
  end

  module Ansi : sig
    include module type of Ansi.Style
  end

  module Duration : sig
    include module type of Duration with type t = Duration.t
    (** @inline *)
  end

  module Printer : sig
    include module type of Printer with type -'a t = 'a Printer.t
    (** @inline *)
  end

  (** Helpers for printing values of various units. *)
  module Units : sig
    include module type of Units
    (** @inline *)
  end

  (** {1 Description} *)

  type 'a reporter = 'a -> unit
  (** A {i reporter} for values of type ['a]. In this library, each progress bar
      has its own reporting function. *)

  (** [Line] contains a DSL for defining custom progress bars. *)
  module Line : sig
    (** @inline *)
    include
      Line.S
        with type 'a t = 'a Line.t
         and type color := Color.t
         and type 'a printer := 'a Printer.t
  end

  (** [Multi] extends [Line] to multi-line layouts. *)
  module Multi : sig
    type ('a, 'b) t
    (** The type of sequences of progress bars. The parameter ['a] stores a list
        of the reporting functions associated with each bar, terminating with
        ['b]. For example:

        {[
          (* Single progress bar, taking a [float] value. *)
          (float reporter -> 'b, 'b) t

          (* A two-bar layout, where the top bar takes [int64]s and the bottom one
             takes [string * float] pairs. *)
          (int64 reporter -> (string * float) reporter -> 'b, 'b) t
        ]}

        These reporting functions are supplied when beginning the {{!rendering}
        rendering} process. *)

    val v : 'a Line.t -> ('a reporter -> 'b, 'b) t
    (** Define a new progress bar from a specification, with the given initial
        value. *)

    val v_list : 'a Line.t list -> ('a reporter list -> 'b, 'b) t

    val ( / ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
    (** Stack progress bars vertically. [a / b] is a set with [a] stacked on top
        of [b]. The two bars have separate reporting functions, passed
        consecutively to the {!with_reporters} continuation when rendering. *)
  end

  (** A list of reporters of differing types. *)
  module Reporters : sig
    type 'a t = [] : unit t | ( :: ) : 'a * 'b t -> ('a -> 'b) t
  end

  (** {1 Rendering} *)

  (** @inline *)
  include
    Renderer.S
      with type 'a line := 'a Line.t
       and type ('a, 'b) multi := ('a, 'b) Multi.t
end

module type Progress_engine = sig
  module type S = S

  module Make (_ : Platform.S) : S
  module Integer = Integer
end

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
