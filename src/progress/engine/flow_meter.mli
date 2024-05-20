(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

type 'a t
(** The type of {i flow meters} for some metric: values that compute an online
    windowed integral of a discrete sequence of [(value, time)] samples. This is
    useful for e.g. estimating the download rate of a process given a sequence
    of progress updates. *)

val create :
     clock:(unit -> Mtime.t)
  -> size:int
  -> elt:(module Integer.S with type t = 'a)
  -> 'a t
(** [create ~clock ~size ~elt] is a flow meter for values of type [elt], using a
    window size of [size] and the [clock] function for collecting timestamps for
    recorded values. *)

val record : 'a t -> 'a -> unit
(** Add a value to the ring buffer. *)

val per_second : 'a t -> float
(** Estimate the rate of change of recorded values per second. *)

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
