open struct
  module Color = Progress.Color
  module Ansi = Progress.Internals.Ansi

  let check_string ~__POS__:pos =
    Alcotest.(check ~pos (testable Fmt.Dump.string String.equal)) ""

  let check_color ~__POS__:pos (r, g, b) x =
    Alcotest.(check ~pos string)
      ""
      (Fmt.str "RGB (%d, %d, %d)" r g b)
      (Fmt.to_to_string Color.pp_dump x)
end

let check_invalid ~__POS__:pos f =
  match f () with
  | _ ->
      Alcotest.fail ~pos
        "Expected [Invalid_argument], but no exception was raised."
  | exception Invalid_argument _ -> ()

let test_rgb () =
  check_invalid ~__POS__ (fun () -> Color.of_rgb (-1) 0 0);
  check_invalid ~__POS__ (fun () -> Color.of_rgb 0 256 0);
  check_string ~__POS__ "\027[38;2;41;42;43m"
    (Color.of_rgb 41 42 43 |> Ansi.fg |> Ansi.code);
  ()

let test_hex () =
  (* Invalid *)
  check_invalid ~__POS__ (fun () -> Color.of_hex "FFFFFF");
  check_invalid ~__POS__ (fun () -> Color.of_hex "#");
  check_invalid ~__POS__ (fun () -> Color.of_hex "#F");
  check_invalid ~__POS__ (fun () -> Color.of_hex "#00000-");
  check_invalid ~__POS__ (fun () -> Color.of_hex "#00-");

  (* Valid *)
  check_color ~__POS__ (0x12, 0x34, 0x56) (Color.of_hex "#123456");
  check_color ~__POS__ (0xab, 0xcd, 0xef) (Color.of_hex "#aBCdEf");

  (* Short aliases *)
  check_color ~__POS__ (0, 0, 0) (Color.of_hex "#000");
  check_color ~__POS__ (17, 34, 51) (Color.of_hex "#123");
  check_color ~__POS__ (255, 255, 255) (Color.of_hex "#fff");

  ()

let tests =
  Alcotest.[ test_case "rgb" `Quick test_rgb; test_case "hex" `Quick test_hex ]
