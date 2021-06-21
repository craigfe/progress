let ( .%() ) v i = Vector.get v i
let ( .%()<- ) v i x = Vector.set v i x

let shuffle_vector =
  let shuffle_subvector rand_int v i j =
    for k = j - 1 downto i + 1 do
      let l = rand_int (k + 1) in
      let tmp = v.%(l) in
      v.%(l) <- v.%(k);
      v.%(k) <- tmp
    done
  in
  fun v -> shuffle_subvector Random.int v 0 (Vector.length v)
