let almost f = f -. Float.epsilon
let ( - ) = Int64.sub

let expect_pp_fixed ?relaxed_length ~pp:pp_fixed s f =
  let result = Progress.Printer.to_to_string pp_fixed f in
  Alcotest.(check string) (Fmt.str "Expected rendering of %s" s) s result;
  match relaxed_length with
  | Some () -> ()
  | None ->
      Alcotest.(check int)
        "Expected length"
        (Progress.Printer.print_width pp_fixed)
        (String.length result)

let test_percentage () =
  let expect = expect_pp_fixed ~pp:Progress.Units.Percentage.of_float in
  expect "  0%" (-0.1);
  expect "  0%" (almost 0.01);
  expect "  1%" 0.01;
  expect "  1%" (0.01 +. Float.epsilon);
  expect " 10%" 0.1;
  expect " 50%" 0.5;
  expect " 99%" (almost 1.);
  expect "100%" 1.;
  expect "100%" (1. +. Float.epsilon);
  expect "100%" 1.1;
  ()

let test_bytes () =
  let expect = expect_pp_fixed ~pp:Progress.Units.Bytes.of_int64 in
  let open Progress.Units.Bytes in
  expect "   0.0 B  " 0L;
  expect " 999.0 B  " 999L;
  expect "   1.0 KiB" (kib 1);
  expect "1023.0 KiB" (mib 1 - kib 1);
  expect "1023.9 KiB" (mib 1 - 1L);
  expect "   1.0 MiB" (mib 1);
  expect "   1.0 TiB" (tib 1);
  expect "   1.0 PiB" (pib 1);
  expect "   1.0 EiB" (pib 1024);
  ()

let test_duration () =
  let module D = Progress.Duration.Of_int in
  let () =
    let expect = expect_pp_fixed ~pp:Progress.Units.Duration.mm_ss in
    expect "00:00" D.(sec 0);
    expect "00:29" D.(sec 29);
    expect "00:30" D.(sec 30);
    expect "00:30" D.(ms 30_400);
    expect "00:59" D.(ms 59_600);
    expect "01:00" D.(min 1);
    expect "01:00" D.(min 1 + ms 400);
    expect "99:00" D.(min 99);
    expect "99:59" D.(min 99 + sec 59);
    (* Fail gracefully: *)
    expect ~relaxed_length:() "100:00" D.(min 100)
  in

  let () =
    let expect = expect_pp_fixed ~pp:Progress.Units.Duration.hh_mm_ss in
    expect "00:00:00" D.(sec 0);
    expect "00:00:59" D.(ms 59_600);
    expect "00:01:00" D.(min 1);
    expect "00:01:00" D.(min 1 + ms 400);
    expect "00:59:59" D.(min 59 + sec 59 + ms 999);
    expect "01:00:00" D.(hour 1);
    expect "01:00:00" D.(hour 1 + ms 1);
    expect "99:59:59" D.(hour 99 + min 59 + sec 59 + ms 999);
    (* Fail gracefully: *)
    expect ~relaxed_length:() "100:00:00" D.(hour 100)
  in

  ()

let tests =
  Alcotest.
    [ test_case "percentage" `Quick test_percentage
    ; test_case "bytes" `Quick test_bytes
    ; test_case "duration" `Quick test_duration
    ]
