open Astring

let ( - ), ( / ) = Int64.(sub, div)

let read_bar () =
  Format.flush_str_formatter ()
  |> String.trim ~drop:(function '\r' | '\n' -> true | _ -> false)

let test_progress_bar_lifecycle () =
  let open Progress.Bytes in
  let report, _bar =
    Progress.start ~ppf:Format.str_formatter
    @@ Progress.counter ~total:(gib 1) ~sampling_interval:1 ~width:60
         ~message:"<msg>" ~pp:Progress.bytes ()
  in
  let check_bar expected =
    read_bar ()
    |> Alcotest.(check string) ("Expected state: " ^ expected) expected
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
  check_bar "<msg>   500.0 MiB  00:00  [############...............]  48%";
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
      Progress.start ~ppf:Format.str_formatter
      @@ Progress.counter ~total:1L ~sampling_interval:1 ~width ~message ?pp ()
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

  Alcotest.check_raises "Overly small progress bar"
    (Invalid_argument "Not enough space for a progress bar") (fun () ->
      ignore
        (Progress.counter ~total:1L ~sampling_interval:1 ~width:18 ~message:""
           ()));
  ()

let () =
  Alcotest.run __FILE__
    [
      ( "main",
        [
          Alcotest.test_case "Progress bar lifecycle" `Quick
            test_progress_bar_lifecycle;
          Alcotest.test_case "Progress bar width" `Quick test_progress_bar_width;
        ] );
    ]
