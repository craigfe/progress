let domains_count = 2

let () =
  let xs = Array.init 100 (Fun.id) in
  let len = Array.length xs in
  let total = if len > 0 then len * (len - 1) / 2 else 0 in
  let bar ~total =
    let open Progress.Line in
    list
      [
        spinner ();
        bar total;
        count_to total;
      ]
  in
  let useful_stuff _i _j = () in
  let module T = Domainslib.Task in
  let pool = T.setup_pool ~num_domains:domains_count () in
  Progress.with_reporter (bar ~total)
    (fun report ->
        T.run pool (fun () ->
            T.parallel_for pool ~start:0 ~finish:(len - 1) ~body:(fun i ->
                T.parallel_for pool ~start:(i + 1) ~finish:(len - 1)
                  ~body:(fun j ->
                    report 1;
                    useful_stuff i j))));
    T.teardown_pool pool