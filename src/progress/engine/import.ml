(*————————————————————————————————————————————————————————————————————————————
   Copyright (c) 2020–2021 Craig Ferguson <me@craigfe.io>
   Distributed under the MIT license. See terms at the end of this file.
  ————————————————————————————————————————————————————————————————————————————*)

include Stdlib_ext
module Terminal = Terminal_ansi

module Mtime = struct
  include Mtime

  let span_to_s span = Mtime.Span.to_float_ns span *. 1e-9
end

module Dynarray = struct
  include Dynarray

  let iter ~f t = iter f t

  let iteri_from ~f i t =
    for i = i to length t - 1 do
      f i (get t i)
    done

  let rec find_map_from i t ~f =
    if i >= length t then None
    else
      let a = get t i in
      match f a with
      | Some _ as some -> some
      | None -> find_map_from (i + 1) t ~f

  let find_map t ~f = find_map_from 0 t ~f

  let insert t k v =
    Dynarray.add_last t v (* Dummy insertion to expand *);
    for i = Dynarray.length t - 1 downto k + 1 do
      Dynarray.set t i (Dynarray.get t (pred i))
    done;
    Dynarray.set t k v

  let remove (type a) (t : a t) k =
    for i = k to Dynarray.length t - 2 do
      Dynarray.set t i (Dynarray.get t (succ i))
    done;
    ignore (Dynarray.pop_last t : a)

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
