  include Vector

  let iter ~f t = iter f t

  let iteri_from ~f i t =
    for i = i to length t - 1 do
      f i (unsafe_get t i)
    done

  let rec find_map_from i t ~f =
    if i >= length t then None
    else
      let a = unsafe_get t i in
      match f a with
      | Some _ as some -> some
      | None -> find_map_from (i + 1) t ~f

  let find_map t ~f = find_map_from 0 t ~f

  let insert t k v =
    Vector.push t v (* Dummy insertion to expand *);
    for i = Vector.length t - 1 downto k + 1 do
      Vector.set t i (Vector.get t (pred i))
    done;
    Vector.set t k v

  let remove (type a) (t : a t) k =
    for i = k to Vector.length t - 2 do
      Vector.set t i (Vector.get t (succ i))
    done;
    ignore (Vector.pop t : a)

  let get_exn = get
  let get = `shadowed