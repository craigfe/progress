let ( .%() ) v i = Dynarray.get v i
let ( .%()<- ) v i x = Dynarray.set v i x

let shuffle_vector =
  let shuffle_subvector rand_int v i j =
    for k = j - 1 downto i + 1 do
      let l = rand_int (k + 1) in
      let tmp = v.%(l) in
      v.%(l) <- v.%(k);
      v.%(k) <- tmp
    done
  in
  fun v -> shuffle_subvector Random.int v 0 (Dynarray.length v)

let colors =
  (* import matplotlib.cm
     for i in matplotlib.cm.rainbow(numpy.linspace(0.2, 1, 20)):
       print(matplotlib.colors.rgb2hex(i))
  *)
  Array.map Progress.Color.hex
    [| "#1996f3"; "#06aeed"; "#10c6e6"; "#27dade"; "#3dead5"
     ; "#52f5cb"; "#66fcc2"; "#7dffb6"; "#92fda9"; "#a8f79c"
     ; "#bced8f"; "#d2de81"; "#e8cb72"; "#feb562"; "#ff9b52"
     ; "#ff8143"; "#ff6232"; "#ff4121"
    |]
[@@ocamlformat "disable"]

let colour_picker () =
  let count = ref (-1) in
  fun () ->
    count := succ !count mod Array.length colors;
    colors.(!count)
