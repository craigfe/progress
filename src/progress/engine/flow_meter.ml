(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

type 'a t =
  { data : 'a array
  ; timestamps : Mtime.t array
  ; max_length : int
  ; mutable most_recently_added : int
  ; mutable length : int
  ; get_time : unit -> Mtime.t
  ; elt : (module Integer.S with type t = 'a)
  }

let create (type a) ~clock:get_time ~size ~elt : a t =
  if size <= 0 then
    Fmt.invalid_arg "Flow_meter.create: non-positive size %d" size;
  (* We need [n + 1] timestamp samples to integrate over [n] values. *)
  let max_length = size + 1 in
  let start_time = get_time () in
  { get_time
  ; data = Array.make max_length (Obj.magic None)
  ; timestamps = Array.make max_length start_time
  ; max_length
  ; most_recently_added = -1
  ; length = 0
  ; elt
  }

let is_empty t = t.most_recently_added = -1

let push t ~key ~data =
  t.data.(key) <- data;
  t.timestamps.(key) <- t.get_time ()

let record t data =
  if t.length = t.max_length then (
    (* Buffer is full. Overwrite the oldest value. *)
    let next = (t.most_recently_added + 1) mod t.length in
    push t ~key:next ~data;
    t.most_recently_added <- next)
  else (
    (* Increase the buffer size *)
    push t ~key:t.length ~data;
    t.most_recently_added <- t.length;
    t.length <- succ t.length)

let oldest_index t =
  if t.length = t.max_length then (t.most_recently_added + 1) mod t.length
  else 0

let fold =
  let rec aux data f acc = function
    | -1 -> acc
    | n -> aux data f (f acc data.(n)) (n - 1)
  in
  fun t ~f ~init -> aux t.data f init (t.length - 1)

let per_second : type a. a t -> a =
 fun t ->
  let (module Integer) = t.elt in
  if is_empty t then Integer.zero
  else
    (* Sum all values in the window {i except the first one} and divide by the
       time interval. We can think of the first value as representing work done
       just {i before} the time interval starts, so using a half-open sample
       correctly avoids over-reporting the flow-rate. *)
    let oldest_index = oldest_index t in
    let sum =
      Integer.sub
        (fold t ~f:Integer.add ~init:Integer.zero)
        t.data.(oldest_index)
    in
    let interval =
      let start_time = t.timestamps.(oldest_index) in
      let end_time = t.timestamps.(t.most_recently_added) in
      Mtime.Span.to_s (Mtime.span start_time end_time)
    in
    if Float.compare interval Float.epsilon < 0 then Integer.zero
    else
      let est = Integer.to_float sum /. interval in
      Integer.of_float est

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
