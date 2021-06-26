open Terminal
open Common

let pp_sty = Fmt.of_to_string Style.code
let col x = Style.fg (Color.ansi x)

let test_guess_width () =
  let check pos ~expected:n fmt =
    Fmt.kstr
      (fun x -> Alcotest.(check ~pos int) "" n (guess_printed_width x))
      fmt
  in
  check __POS__ ~expected:0 "";
  check __POS__ ~expected:1 "a";
  check __POS__ ~expected:5 "  a  ";
  check __POS__ ~expected:3 "▷▷▷";

  check __POS__ ~expected:0 "%a" pp_sty (col `red);
  check __POS__ ~expected:3 "%a<->%a" pp_sty (col `red) pp_sty (col `green);
  ()

let test_truncate () =
  let trunc n fmt = Fmt.kstr (truncate_to_width n) fmt in
  let check pos ~expected s =
    Alcotest.(check ~pos (testable Fmt.Dump.string ( = ))) "" expected s
  in
  check __POS__ ~expected:"" (trunc 0 "");
  check __POS__ ~expected:"" (trunc 5 "");
  check __POS__ ~expected:"▷" (trunc 1 "▷▷▷");
  check __POS__ ~expected:"▷▷" (trunc 2 "▷▷▷");
  check __POS__ ~expected:"▷▷▷" (trunc 3 "▷▷▷");

  check __POS__ ~expected:"" (trunc 0 "%a" pp_sty (col `red));
  check __POS__ ~expected:"\027[31m" (trunc 1 "%a" pp_sty (col `red));
  check __POS__ ~expected:"▷\027[31m" (trunc 2 "▷%a" pp_sty (col `red));

  (* Trailing colour is trimmed: *)
  check __POS__ ~expected:"▷" (trunc 1 "▷%a" pp_sty (col `red));

  (* Trailing reset is not trimmed: *)
  check __POS__ ~expected:"▷\027[0m" (trunc 1 "▷%a" pp_sty Style.none);

  (* Trailing bytes look like a reset, but aren't: *)
  check __POS__ ~expected:"▷" (trunc 1 "▷\027[0");

  check_invalid __POS__ (fun () -> trunc (-1) "");
  ()

let tests =
  Alcotest.
    [ test_case "guess_width" `Quick test_guess_width
    ; test_case "truncate" `Quick test_truncate
    ]
