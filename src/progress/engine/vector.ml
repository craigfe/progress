type 'a t =
  { mutable cardinal : int
  ; mutable data : 'a array (* 0 <= cardinal <= Array.length data *)
  }

let magic_array n =
  Array.make n (Obj.magic `I_solemnly_swear_that_I_am_up_to_no_good)

let create n =
  if n < 0 || n > Sys.max_array_length then invalid_arg "Vector.make";
  { cardinal = n; data = magic_array n }

let of_list l =
  let arr = Array.of_list l in
  { cardinal = Array.length arr; data = arr }

let of_array arr = { cardinal = Array.length arr; data = Array.copy arr }
let length t = t.cardinal

let get_exn t i =
  if i < 0 || i >= t.cardinal then invalid_arg "Vector.get";
  Array.unsafe_get t.data i

let unsafe_get t i = Array.unsafe_get t.data i
let unsafe_set t i a = Array.unsafe_set t.data i a
let clamp ~lower ~upper x = min (max x lower) upper

let resize t cardinal' =
  if cardinal' < 0 then invalid_arg "Vector.resize";
  let len = Array.length t.data in
  if cardinal' > len then (
    if cardinal' > Sys.max_array_length then
      invalid_arg "Vector.resize: cannot grow";
    let len' = clamp ~lower:cardinal' ~upper:Sys.max_array_length (2 * len) in
    let data' = magic_array len' in
    Array.blit t.data 0 data' 0 t.cardinal;
    t.data <- data');
  t.cardinal <- cardinal'

let insert t i a =
  let cardinal = t.cardinal in
  if i < 0 || i > cardinal then invalid_arg "Vector.resize";
  resize t (succ cardinal);
  for j = cardinal downto i + 1 do
    unsafe_set t j (unsafe_get t (pred j))
  done;
  unsafe_set t i a

let iter_from from a ~f =
  for i = from to length a - 1 do
    f (unsafe_get a i)
  done

let iter a ~f = iter_from 0 a ~f

let iteri_from from a ~f =
  for i = from to length a - 1 do
    f i (unsafe_get a i)
  done

let iteri a ~f = iteri_from 0 a ~f
