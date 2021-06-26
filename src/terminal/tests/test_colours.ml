open Terminal
open Common

let check_string pos = Alcotest.(check ~pos (testable Fmt.Dump.string ( = ))) ""

let check_color pos (r, g, b) x =
  Alcotest.(check ~pos string)
    ""
    (Fmt.str "RGB (%d, %d, %d)" r g b)
    (Fmt.to_to_string Color.pp_dump x)

let test_rgb () =
  check_invalid __POS__ (fun () -> Color.rgb (-1) 0 0);
  check_invalid __POS__ (fun () -> Color.rgb 0 256 0);
  check_string __POS__ "\027[38;2;41;42;43m"
    (Color.rgb 41 42 43 |> Style.fg |> Style.code);
  ()

let test_hex () =
  (* Invalid *)
  check_invalid __POS__ (fun () -> Color.hex "FFFFFF");
  check_invalid __POS__ (fun () -> Color.hex "#");
  check_invalid __POS__ (fun () -> Color.hex "#F");
  check_invalid __POS__ (fun () -> Color.hex "#00000-");
  check_invalid __POS__ (fun () -> Color.hex "#00-");

  (* Valid *)
  check_color __POS__ (0x12, 0x34, 0x56) (Color.hex "#123456");
  check_color __POS__ (0xab, 0xcd, 0xef) (Color.hex "#aBCdEf");

  (* Short aliases *)
  check_color __POS__ (0, 0, 0) (Color.hex "#000");
  check_color __POS__ (17, 34, 51) (Color.hex "#123");
  check_color __POS__ (255, 255, 255) (Color.hex "#fff");

  ()

let tests =
  Alcotest.[ test_case "rgb" `Quick test_rgb; test_case "hex" `Quick test_hex ]
