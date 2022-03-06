(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Stdlib_ext

module Mtime = struct
  include Mtime

  let span_to_s span = Mtime.Span.to_float_ns span *. 1e-9
end

module Vector = struct
  include Vector

  let iter ~f t = iter f t

  let iteri_from ~f i t =
    for i = i to length t - 1 do
      f i (unsafe_get t i)
    done

  let insert t k v =
    Vector.push t v (* Dummy insertion to expand *);
    for i = Vector.length t - 1 downto k + 1 do
      Vector.set t i (Vector.get t (pred i))
    done;
    Vector.set t k v

  let remove t k =
    for i = k to Vector.length t - 2 do
      Vector.set t i (Vector.get t (succ i))
    done;
    ignore (Vector.pop t)

  let get_exn = get
  let get = `shadowed
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
