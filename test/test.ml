open Astring

let ( - ), ( / ) = Int64.(sub, div)
let almost f = f -. Float.epsilon
let ( let@ ) f x = f x

let config =
  Progress.Config.create ~ppf:Format.str_formatter ~hide_cursor:false ()

let read_bar () =
  Format.flush_str_formatter ()
  |> String.trim ~drop:(function '\r' | '\n' -> true | _ -> false)

let check_bar expected =
  Alcotest.check Alcotest.string
    ("Expected state: " ^ expected)
    expected (read_bar ())

let clear_test_state () = ignore (Format.flush_str_formatter () : string)

let check_no_bar () =
  Format.flush_str_formatter () |> Alcotest.(check string) "Expected no bar" ""

module Units = struct end

let test_pair () =
  let bar =
    Progress.(
      Segment.(
        pair ~sep:(const ", ")
          (of_pp ~width:1 Format.pp_print_int)
          (of_pp ~width:1 Format.pp_print_string))
      |> make ~init:(0, "foo"))
  in
  let () =
    let@ report = Progress.with_reporters ~config bar in
    check_bar "0, foo";
    report (1, "bar");
    check_bar "1, bar"
  in
  check_bar "1, bar"

let test_unicode_bar () =
  let () =
    let@ report =
      Progress.(
        Segment.bar ~style:`UTF8 ~width:(`Fixed 3) Fun.id
        |> make ~init:0.
        |> with_reporters ~config)
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
    expect "│█│" (almost 2.)
  in
  clear_test_state ();
  let () =
    let@ report =
      Progress.Segment.bar ~style:`UTF8 ~width:(`Fixed 5) Fun.id
      |> Progress.make ~init:0.
      |> Progress.with_reporters ~config
    in
    let expect s f =
      report f;
      check_bar s
    in
    check_bar "│   │";
    expect "│   │" 0.;
    expect "│█▌ │" 0.5;
    expect "│██▉│" (almost 1.);
    expect "│███│" 1.
  in
  ()

let test_progress_bar_lifecycle () =
  let open Progress.Units.Bytes in
  let@ report =
    Progress.counter ~style:`ASCII ~total:(gib 1) ~sampling_interval:1 ~width:53
      ~message:"<msg>" ~pp:(of_int64, width)
      (module Int64)
    |> Progress.with_reporters ~config
  in
  check_bar "<msg>     0.0 B    [---------------------------]   0%";
  report (kib 1 - 1L);
  check_bar "<msg>  1023.0 B    [---------------------------]   0%";
  report 1L;
  check_bar "<msg>     1.0 KiB  [---------------------------]   0%";
  report (mib 1 - kib 1 - 1L);
  (* Should always round downwards. *)
  check_bar "<msg>  1023.9 KiB  [---------------------------]   0%";
  report 1L;
  check_bar "<msg>     1.0 MiB  [---------------------------]   0%";
  report (mib 49);
  check_bar "<msg>    50.0 MiB  [#--------------------------]   4%";
  report (mib 450);
  check_bar "<msg>   500.0 MiB  [#############--------------]  48%";
  report (gib 1 - mib 500 - 1L);
  (* 1 byte from completion. Should show 99% and not a full 1024 MiB. *)
  check_bar "<msg>  1023.9 MiB  [##########################-]  99%";
  report 1L;
  (* Now exactly complete *)
  check_bar "<msg>     1.0 GiB  [###########################] 100%";
  (* Subsequent reports don't overflow the bar *)
  report (gib 1 / 2L);
  check_bar "<msg>     1.5 GiB  [###########################] 100%";
  ()

let test_progress_bar_width () =
  let check_width ~width ~message ?pp ~count_width () =
    let@ _report =
      Progress.with_reporters ~config
        (Progress.counter ~style:`ASCII ~total:1L ~sampling_interval:1 ~width
           ~message ?pp
           (module Int64))
    in
    String.length (read_bar ())
    |> Alcotest.(check int)
         (Fmt.str
            "Expected width for configuration { width = %d; count_width = %d; \
             message = %S }"
            width count_width message)
         width
  in
  check_width ~width:80 ~message:"<msg>"
    ~pp:Progress.Units.Bytes.(of_int64, width)
    ~count_width:5 ();
  clear_test_state ();
  check_width ~width:40 ~message:""
    ~pp:(Fmt.(const string "XX"), 2)
    ~count_width:2 ();
  clear_test_state ();
  check_width ~width:40 ~message:"Very long message" ~count_width:0 ();

  (* TODO: Static vs Dynamic distinction in expandable
   *
   * Alcotest.check_raises "Overly small progress bar"
   *   (Invalid_argument "Not enough space for a progress bar") (fun () ->
   *     ignore
   *       (Progress.counter ~total:1L ~sampling_interval:1 ~width:18 ~message:""
   *          ())); *)
  ()

module Boxes = struct
  let test_unsized_not_in_box () =
    Alcotest.check_raises "Unsized element not contained in a box"
      (Invalid_argument
         "Encountered an expanding element that is not contained in a box")
    @@ fun () ->
    ignore
      Progress.(
        start @@ make ~init:0. Segment.(bar ~width:`Expand ~style:`UTF8 Fun.id))

  let test_two_unsized_in_box () =
    Alcotest.check_raises "Two unsized elements in a box"
      (Invalid_argument
         "Multiple expansion points encountered. Cannot pack two unsized \
          segments in a single box.")
    @@ fun () ->
    let open Progress in
    let unsized = Segment.bar ~style:`UTF8 Fun.id in
    ignore (start @@ make ~init:0. Segment.(box_fixed 10 (unsized ++ unsized)))
end

module Stateful = struct
  let test_periodic () =
    let@ report =
      Progress.(
        Segment.(accumulator ( + ) 0 (periodic 3 (of_pp ~width:1 Fmt.int)))
        |> make ~init:0
        |> with_reporters ~config)
    in
    check_bar "0";
    report 1;
    check_no_bar ();
    report 1;
    check_no_bar ();
    report 1;
    check_bar "3";
    report 10;
    check_no_bar ();
    report 10;
    check_no_bar ();
    report 10;
    check_bar "33";
    ()
end

let () =
  let open Alcotest in
  run __FILE__
    [ ( "main"
      , [ test_case "Pair" `Quick test_pair
        ; test_case "Unicode bar" `Quick test_unicode_bar
        ; test_case "Progress bar lifecycle" `Quick test_progress_bar_lifecycle
        ; test_case "Progress bar width" `Quick test_progress_bar_width
        ] )
    ; ( "boxes"
      , [ test_case "Unsized element not in box" `Quick
            Boxes.test_unsized_not_in_box
        ; test_case "Two unsized elements in box" `Quick
            Boxes.test_two_unsized_in_box
        ] )
    ; ("stateful", [ test_case "periodic" `Quick Stateful.test_periodic ])
    ; ("units", Test_units.tests)
    ]
