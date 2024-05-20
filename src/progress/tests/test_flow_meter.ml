open struct
  module Flow_meter = Progress_engine.Exposed_for_testing.Flow_meter
  module Duration = Progress.Duration
end

let gen_test_state () =
  let now = ref (Mtime.of_uint64_ns 0L) in
  let clock () = !now in
  let flow_meter = Flow_meter.create ~clock ~size:3 ~elt:(module Int) in
  let record ~interval x =
    now := Option.get (Mtime.add_span !now interval);
    Flow_meter.record flow_meter x
  in
  let check pos v =
    Alcotest.(check ~pos int)
      "Expected flow rate" v
      (Flow_meter.per_second flow_meter |> Int.of_float)
  in
  (record, check)

(* Tests of flow rate in which samples are all 1ms apart. *)
let test_linear () =
  let record, check = gen_test_state () in
  let record = record ~interval:Duration.millisecond in
  (* Initial flow is 0 *)
  check __POS__ 0;
  check __POS__ 0;
  (* Flow rate is 0 until we have at least 2 samples *)
  record 1;
  check __POS__ 0;
  record 1;
  check __POS__ 1000 (* 1 in 1 ms *);
  record 0;
  check __POS__ 500 (* (1 + 0) in 2 ms *);
  record 1;
  check __POS__ 666 (* (1 + 0 + 1) in 3 ms *);
  record 1;
  check __POS__ 666 (* (0 + 1 + 1) in 3 ms *);
  record 1;
  check __POS__ 1000 (* (1 + 1 + 1) in 3 ms *);
  record (-1);
  check __POS__ 333 (* (1 + 1 + -1) in 3 ms *);
  record (-1);
  check __POS__ (-333) (* (1 + -1 + -1) in 3 ms *);
  record 2;
  check __POS__ 0 (* (-1 + -1 + 2) in 3 ms *);
  ()

let test_non_linear () =
  let record, check = gen_test_state () in
  (* Initial flow is 0 *)
  check __POS__ 0;
  check __POS__ 0;
  (* Flow rate is 0 until we have at least 2 samples *)
  record ~interval:Duration.zero 1;
  record ~interval:Duration.zero 1;
  record ~interval:Duration.zero 1;
  check __POS__ 0 (* 3 in 0 ms *);
  record ~interval:Duration.millisecond 0;
  check __POS__ 2000 (* (1 + 1 + 0) in 1 ms *);
  record ~interval:Duration.zero 0;
  check __POS__ 1000 (* (1 + 0 + 0) in 1 ms *);
  record ~interval:Duration.zero 0;
  check __POS__ 0 (* (0 + 0 + 0) in 1 ms *);
  record ~interval:Duration.zero 3;
  check __POS__ 0 (* (0 + 0 + 3) in 0 ms *);
  ()

let tests =
  [ ("linear", `Quick, test_linear); ("non-linear", `Quick, test_non_linear) ]
