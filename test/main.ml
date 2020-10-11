open Astring

let ( - ), ( / ) = Int64.(sub, div)
let almost f = f -. Float.epsilon
let ppf = Format.str_formatter

let read_bar () =
  Format.flush_str_formatter ()
  |> String.trim ~drop:(function '\r' | '\n' -> true | _ -> false)

let check_bar expected =
  read_bar ()
  |> Alcotest.(check string) ("Expected state: " ^ expected) expected

let test_percentage () =
  let report, _ = Progress.(Segment.percentage |> v ~init:0. |> start ~ppf) in
  let expect s f =
    report f;
    check_bar s
  in
  check_bar "  0%";
  expect "  0%" (almost 0.01);
  expect "  1%" 0.01;
  expect "  1%" (0.01 +. Float.epsilon);
  expect " 10%" 0.1;
  expect " 50%" 0.5;
  expect " 99%" 0.99;
  expect " 99%" (almost 1.);
  expect "100%" 1.;
  expect "100%" (1. +. Float.epsilon);
  expect "100%" 1.1;
  expect "  0%" (-0.1);
  ()

let test_pair () =
  let bar =
    Progress.(
      Segment.(
        pair ~sep:(const ", ")
          (fmt ~width:1 Format.pp_print_int)
          (fmt ~width:3 Format.pp_print_string))
      |> v ~init:(0, "foo"))
  in
  Progress.with_display ~ppf bar (fun report ->
      check_bar "0, foo";
      report (1, "bar");
      check_bar "1, bar");
  check_bar "1, bar"

let test_unicode_bar () =
  let report, _ =
    Progress.(
      Segment.bar ~mode:`UTF ~width:(`Fixed 3) Fun.id
      |> v ~init:0.
      |> start ~ppf)
  in
  let expect s f =
    report f;
    check_bar s
  in
  check_bar "│ │";
  expect "│ │" 0.;
  expect "│ │" (almost (1. /. 8.));
  expect "│▏│" (1. /. 8.);
  expect "│▏│" (almost (2. /. 8.));
  expect "│▎│" (2. /. 8.);
  expect "│▎│" (almost (3. /. 8.));
  expect "│▍│" (3. /. 8.);
  expect "│▍│" (almost (4. /. 8.));
  expect "│▌│" (4. /. 8.);
  expect "│▌│" (almost (5. /. 8.));
  expect "│▋│" (5. /. 8.);
  expect "│▋│" (almost (6. /. 8.));
  expect "│▊│" (6. /. 8.);
  expect "│▊│" (almost (7. /. 8.));
  expect "│▉│" (7. /. 8.);
  expect "│▉│" (almost 1.);
  expect "│█│" 1.;
  expect "│█│" (1. +. Float.epsilon);
  expect "│█│" (1. +. (1. /. 8.));
  expect "│█│" (almost 2.);
  let report, _ =
    Progress.Segment.bar ~mode:`UTF ~width:(`Fixed 5) Fun.id
    |> Progress.v ~init:0.
    |> Progress.start ~ppf
  in
  let expect s f =
    report f;
    check_bar s
  in
  check_bar "│   │";
  expect "│   │" 0.;
  expect "│█▌ │" 0.5;
  expect "│██▉│" (almost 1.);
  expect "│███│" 1.;
  ()

let test_progress_bar_lifecycle () =
  let open Progress.Bytes in
  let report, _bar =
    Progress.start ~ppf
    @@ Progress.counter ~mode:`ASCII ~total:(gib 1) ~sampling_interval:1
         ~width:60 ~message:"<msg>" ~pp:Progress.bytes ()
  in
  check_bar "<msg>     0.0 B    00:00  [...........................]   0%";
  report (kib 1 - 1L);
  check_bar "<msg>  1023.0 B    00:00  [...........................]   0%";
  report 1L;
  check_bar "<msg>     1.0 KiB  00:00  [...........................]   0%";
  report (mib 1 - kib 1 - 1L);
  (* Should always round downwards. *)
  check_bar "<msg>  1023.9 KiB  00:00  [...........................]   0%";
  report 1L;
  check_bar "<msg>     1.0 MiB  00:00  [...........................]   0%";
  report (mib 49);
  check_bar "<msg>    50.0 MiB  00:00  [#..........................]   4%";
  report (mib 450);
  check_bar "<msg>   500.0 MiB  00:00  [#############..............]  48%";
  report (gib 1 - mib 500 - 1L);
  (* 1 byte from completion. Should show 99% and not a full 1024 MiB. *)
  check_bar "<msg>  1023.9 MiB  00:00  [##########################.]  99%";
  report 1L;
  (* Now exactly complete *)
  check_bar "<msg>     1.0 GiB  00:00  [###########################] 100%";
  (* Subsequent reports don't overflow the bar *)
  report (gib 1 / 2L);
  check_bar "<msg>     1.5 GiB  00:00  [###########################] 100%";
  ()

let test_progress_bar_width () =
  let check_width ~width ~message ?pp () =
    let _, _ =
      Progress.start ~ppf
        (Progress.counter ~mode:`ASCII ~total:1L ~sampling_interval:1 ~width
           ~message ?pp ())
    in
    let count_width = match pp with Some (_, c) -> c | None -> 0 in
    String.length (read_bar ())
    |> Alcotest.(check int)
         (Fmt.str
            "Expected width for configuration { width = %d; count_width = %d; \
             message = %s }"
            width count_width message)
         width
  in
  check_width ~width:80 ~message:"<msg>" ~pp:Progress.bytes ();
  check_width ~width:40 ~message:"" ~pp:(Fmt.(const string "XX"), 2) ();
  check_width ~width:40 ~message:"Very long message" ();

  (* TODO: Static vs Dynamic distinction in expandable
   *
   * Alcotest.check_raises "Overly small progress bar"
   *   (Invalid_argument "Not enough space for a progress bar") (fun () ->
   *     ignore
   *       (Progress.counter ~total:1L ~sampling_interval:1 ~width:18 ~message:""
   *          ())); *)
  ()

let test_unsized_not_in_box () =
  Alcotest.check_raises "Unsized element not contained in a box"
    (Invalid_argument
       "Encountered an expanding element that is not contained in a box")
    (fun () ->
      let open Progress in
      ignore (Segment.(bar ~mode:`UTF Int64.to_float) |> v ~init:0L |> start))

let () =
  let open Alcotest in
  run __FILE__
    [
      ( "main",
        [
          test_case "Percentage" `Quick test_percentage;
          test_case "Pair" `Quick test_pair;
          test_case "Unicode bar" `Quick test_unicode_bar;
          test_case "Progress bar lifecycle" `Quick test_progress_bar_lifecycle;
          test_case "Progress bar width" `Quick test_progress_bar_width;
        ] );
      ( "boxes",
        [
          test_case "Unsized element not in box" `Quick test_unsized_not_in_box;
        ] );
    ]
