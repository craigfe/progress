(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

(** A library for displaying progress bars, including support for rendering
    multiple bars at once. Start by {{!description} describing} of a sequence of
    progress bars, then begin {{!rendering} rendering} them.

    {[
      (** Description: "⠼️ [###########--------------------] 37/100" *)
      let bar ~total =
        let open Progress.Line in
        list [ spinner (); bar total; count_to total ]

      (** Rendering: get access to a function [f] for reporting progress. *)
      let run () =
        Progress.with_reporter (bar ~total:100) (fun f ->
            for i = 1 to 100 do
              (* ... do some work ... *)
              f 1 (* report some progress *)
            done)
    ]}

    See {!Progress_engine} for an equivalent API that is portable to non-Unix
    platforms. *)

include Progress_engine.S
(** @inline *)

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
