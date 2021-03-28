type t =
  { data : int64 array
  ; timestamps : Mtime.t array
  ; max_length : int
  ; mutable most_recently_added : int
  ; mutable length : int
  }

let get_time () = Mtime_clock.now ()

let create ~size:max_length =
  let start_time = get_time () in
  { data = Array.make max_length 0L
  ; timestamps = Array.make max_length start_time
  ; max_length
  ; most_recently_added = -1
  ; length = 0
  }

let is_empty t = t.most_recently_added = -1

let push t ~key ~data =
  t.data.(key) <- data;
  t.timestamps.(key) <- get_time ()

let record t data =
  if t.length = t.max_length then (
    (* Buffer is full. Overwrite the oldest value. *)
    let next = (t.most_recently_added + 1) mod t.length in
    push t ~key:next ~data;
    t.most_recently_added <- next)
  else (
    (* Increase the buffer size *)
    push t ~key:t.length ~data;
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

let rate_per_second t =
  if is_empty t then 0.
  else
    let interval =
      let start_time = t.timestamps.(oldest_index t) in
      let end_time = t.timestamps.(t.most_recently_added) in
      Mtime.Span.to_s (Mtime.span start_time end_time)
    in
    let sum = Int64.to_float (fold t ~f:Int64.add ~init:0L) in
    sum /. interval
