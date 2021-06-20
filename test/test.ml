open Astring

let ( -- ), ( // ) = Int64.(sub, div)
let almost f = f -. Float.epsilon
let ( let@ ) f x = f x

let config =
  Progress.Config.v ~ppf:Format.str_formatter ~hide_cursor:false
    ~persistent:true ~min_interval:None ()

let read_bar () =
  Format.flush_str_formatter ()
  |> String.trim ~drop:(function '\r' | '\n' -> true | _ -> false)

let check_bar expected =
  Alcotest.check
    Alcotest.(testable Fmt.Dump.string String.equal)
    ("Expected state: " ^ expected)
    expected (read_bar ())

let clear_test_state () = ignore (Format.flush_str_formatter () : string)

let test_pair () =
  let bar =
    Progress.(
      Line.(
        pair ~sep:(const ", ")
          (of_printer ~init:0 (Printer.int ~width:1))
          (of_printer ~init:"foo" (Printer.string ~width:3))))
  in
  let () =
    let@ report = Progress.with_reporter ~config bar in
    check_bar "0, foo";
    report (1, "bar");
    check_bar "1, bar"
  in
  check_bar "1, bar"

let test_unicode_bar () =
  let () =
    let@ report =
      Progress.Line.Using_float.bar_unaccumulated ~style:`UTF8 ~width:(`Fixed 3)
        ~total:1. ()
      |> Progress.with_reporter ~config
    in
    let expect s f =
      report f;
      check_bar s
    in
    check_bar "│ │";
    expect "" 0.;
    expect "" (almost (1. /. 8.));
    expect "│▏│" (1. /. 8.);
    expect "" (almost (2. /. 8.));
    expect "│▎│" (2. /. 8.);
    expect "" (almost (3. /. 8.));
    expect "│▍│" (3. /. 8.);
    expect "" (almost (4. /. 8.));
    expect "│▌│" (4. /. 8.);
    expect "" (almost (5. /. 8.));
    expect "│▋│" (5. /. 8.);
    expect "" (almost (6. /. 8.));
    expect "│▊│" (6. /. 8.);
    expect "" (almost (7. /. 8.));
    expect "│▉│" (7. /. 8.);
    expect "" (almost 1.);
    expect "│█│" 1.;
    expect "" (1. +. Float.epsilon);
    expect "" (1. +. (1. /. 8.));
    expect "" (almost 2.)
  in
  clear_test_state ();
  let () =
    let@ report =
      Progress.Line.Using_float.bar_unaccumulated ~style:`UTF8 ~width:(`Fixed 5)
        ~total:1. ()
      |> Progress.with_reporter ~config
    in
    let expect s f =
      report f;
      check_bar s
    in
    check_bar "│   │";
    expect "" 0.;
    expect "│█▌ │" 0.5;
    expect "│██▉│" (almost 1.);
    expect "│███│" 1.
  in
  ()

let test_progress_bar_lifecycle () =
  let open Progress.Units.Bytes in
  let@ report =
    let open Progress.Line.Using_int64 in
    list
      [ const "<msg>"
      ; bytes
      ; bar ~style:`ASCII ~width:(`Fixed 29) ~total:(gib 1) ()
        ++ const " "
        ++ percentage_of (gib 1)
      ]
    |> Progress.with_reporter ~config
  in
  check_bar "<msg>    0.0 B   [---------------------------]   0%";
  report (kib 1 -- 1L);
  check_bar "<msg> 1023.0 B   [---------------------------]   0%";
  report 1L;
  check_bar "<msg>    1.0 KiB [---------------------------]   0%";
  report (mib 1 -- kib 1 -- 1L);
  (* Should always round downwards. *)
  check_bar "<msg> 1023.9 KiB [---------------------------]   0%";
  report 1L;
  check_bar "<msg>    1.0 MiB [---------------------------]   0%";
  report (mib 49);
  check_bar "<msg>   50.0 MiB [#--------------------------]   4%";
  report (mib 450);
  check_bar "<msg>  500.0 MiB [#############--------------]  48%";
  report (gib 1 -- mib 500 -- 1L);
  (* 1 byte from completion. Should show 99% and not a full 1024 MiB. *)
  check_bar "<msg> 1023.9 MiB [##########################-]  99%";
  report 1L;
  (* Now exactly complete *)
  check_bar "<msg>    1.0 GiB [###########################] 100%";
  (* Subsequent reports don't overflow the bar *)
  report (gib 1 // 2L);
  check_bar "<msg>    1.5 GiB [###########################] 100%";
  ()

let test_progress_bar_width () =
  let check_width width =
    clear_test_state ();
    let@ _report =
      let open Progress.Line.Using_int64 in
      Progress.with_reporter ~config
        (bar ~style:`ASCII ~width:(`Fixed width) ~total:1L ())
      (* Progress.with_reporters ~config
       *   (Progress.counter ~style:`ASCII ~total:1L ~sampling_interval:1 ~width
       *      ~message ?pp
       *      (module Int64)) *)
    in
    let s = read_bar () in
    String.length s
    |> Alcotest.(check int) (Fmt.str "Expected width of %d: `%S`" width s) width
  in
  check_width 80;
  check_width 40;
  Alcotest.check_raises "Overly small progress bar"
    (Failure "Not enough space for a progress bar") (fun () -> check_width 2)

module Boxes = struct
  let unsized =
    Progress.Line.Internals.alpha_unsized ~initial:(`Val ())
      (fun ~width:_ _ _ -> 0)

  let test_unsized_not_in_box () =
    Alcotest.check_raises "Unsized element not contained in a box"
      (Invalid_argument
         "Encountered an expanding element that is not contained in a box")
    @@ fun () ->
    Progress.(with_reporter Line.Internals.(to_line unsized) Fun.id ())

  let test_two_unsized_in_box () =
    Alcotest.check_raises "Two unsized elements in a box"
      (Invalid_argument
         "Multiple expansion points encountered. Cannot pack two unsized \
          segments in a single box.")
    @@ fun () ->
    Progress.(
      with_reporter
        Line.Internals.(to_line @@ box_fixed 10 (array [| unsized; unsized |]))
        Fun.id ())
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
    ; ("line", Test_line.tests)
    ; ("units", Test_units.tests)
    ; ("colours", Test_colours.tests)
    ; ("printers", Test_printers.tests)
    ]
