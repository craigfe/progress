type 'a t =
  { data : 'a array
  ; timestamps : Mtime.t array
  ; max_length : int
  ; mutable most_recently_added : int
  ; mutable length : int
  ; get_time : unit -> Mtime.t
  ; elt : (module Integer.S with type t = 'a)
  }

let create (type a) ~clock:get_time ~size:max_length ~elt : a t =
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

let rate_per_second : type a. a t -> a =
 fun t ->
  let (module Integer) = t.elt in
  if is_empty t then Integer.zero
  else
    let interval =
      let start_time = t.timestamps.(oldest_index t) in
      let end_time = t.timestamps.(t.most_recently_added) in
      Mtime.Span.to_s (Mtime.span start_time end_time)
    in
    let sum = fold t ~f:Integer.add ~init:Integer.zero in
    let est = Integer.to_float sum /. interval in
    Integer.of_float est
