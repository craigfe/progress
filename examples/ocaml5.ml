(* Run as: time dune exec examples/ocaml5.exe -- -j 4 -l 1000  *)
type cfg = { mutable num_domains : int; mutable length : int }

let cfg = { num_domains = 4; length = 100 }

let () =
  Arg.parse
    [ ("-j", Arg.Int (fun n -> cfg.num_domains <- n), " number of domains")
    ; ("-l", Arg.Int (fun n -> cfg.length <- n), " array length")
    ]
    (fun _ -> assert false)
    ""

let rec slow_fib n = if n <= 1 then n else slow_fib (n - 2) + slow_fib (n - 1)

let () =
  (* Quadratic number of iterations *)
  let total = if cfg.length > 0 then cfg.length * (cfg.length - 1) / 2 else 0 in
  let bar ~total =
    let open Progress.Line in
    list [ spinner (); bar total; count_to total ]
  in
  let m = Mutex.create () in
  let useful_stuff report _i _j =
    assert (0 <= abs (slow_fib 25));
    Mutex.protect m (fun () -> report 1)
  in
  let module T = Domainslib.Task in
  let pool = T.setup_pool ~num_domains:cfg.num_domains () in
  Progress.with_reporter
    (* ~config:(Progress.Config.v ~ppf:(Format.formatter_of_out_channel stdout) ()) *)
    (bar ~total) (fun report ->
      T.run pool (fun () ->
          T.parallel_for pool ~start:0 ~finish:(cfg.length - 1) ~body:(fun i ->
              T.parallel_for pool ~start:(i + 1) ~finish:(cfg.length - 1)
                ~body:(fun j -> useful_stuff report i j))));
  T.teardown_pool pool
