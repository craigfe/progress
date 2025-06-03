include Dynarray

let iter ~f t = iter f t

let iteri_from ~f i t =
  for i = i to length t - 1 do
    f i (get t i)
  done

let of_list ~dummy:_ l = Dynarray.of_list l

let rec find_map_from i t ~f =
  if i >= length t then None
  else
    let a = get t i in
    match f a with Some _ as some -> some | None -> find_map_from (i + 1) t ~f

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
