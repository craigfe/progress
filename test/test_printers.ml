open Common

open struct
  module Printer = Progress.Printer
  module Color = Progress.Color
  module Ansi = Progress.Internals.Ansi

  let check_print print ~__POS__:pos str x =
    Alcotest.(check ~pos (testable Fmt.Dump.string String.equal))
      "" str (print x)
end

let test_int () =
  let print = Printer.(to_to_string (int ~width:5)) in
  let check = check_print print in
  check ~__POS__ "12345" 12345;
  check ~__POS__ "-2345" (-2345);
  check ~__POS__ "    1" 1;
  check ~__POS__ "   -1" (-1);
  check_invalid ~__POS__ (fun () -> print 123456);
  check_invalid ~__POS__ (fun () -> print (-23456));
  ()

let test_string () =
  let check ?(width = 8) = check_print Printer.(to_to_string (string ~width)) in

  (* ASCII strings *)
  check ~__POS__ "        " "";
  check ~__POS__ "hello   " "hello";
  check ~__POS__ "  hello " "  hello";
  check ~__POS__ "hello me" "hello me";
  check ~__POS__ "hello..." "hello world";

  (* Short ASCII strings (within maximum ellipsis size) *)
  check ~__POS__ ~width:3 "abc" "abc";
  check ~__POS__ ~width:3 "..." "abcd";
  check ~__POS__ ~width:2 "ab" "ab";
  check ~__POS__ ~width:2 ".." "abc";
  check ~__POS__ ~width:1 "a" "a";
  check ~__POS__ ~width:1 "." "ab";
  check ~__POS__ ~width:0 "" "";
  check ~__POS__ ~width:0 "" "a";

  (* Non-ASCII UTF8 strings *)
  check ~__POS__ "————————" "————————";
  check ~__POS__ "—————..." "—————————";
  check ~__POS__ ~width:3 "———" "———";
  check ~__POS__ ~width:3 "..." "————";
  check ~__POS__ ~width:2 ".." "———";
  check ~__POS__ ~width:1 "." "——";

  (* Strings containing ANSI colour escapes *)
  let () =
    let col c s = Ansi.(code (fg (Color.ansi c))) ^ s ^ Ansi.(code none) in
    (* Build up a coloured "hello world" string, retaining prefixes *)
    let h = col `red "h" in
    let he = h ^ col `blue "e" in
    let hel = he ^ col `green "l" in
    let hell = hel ^ "l" in
    let hello = hell ^ col `red "o" in
    let hello_world = hello ^ col `magenta " " ^ col `yellow "w" ^ "orld" in

    (* Check that ppadding is applied correctly *)
    check ~width:13 ~__POS__ (hello_world ^ "  ") hello_world;
    check ~width:12 ~__POS__ (hello_world ^ " ") hello_world;
    check ~width:11 ~__POS__ hello_world hello_world;

    (* Check that it truncates correctly at each point *)
    check ~width:8 ~__POS__ (hello ^ "...") hello_world;
    check ~width:7 ~__POS__ (hell ^ "...") hello_world;
    check ~width:6 ~__POS__ (hel ^ "...") hello_world;
    check ~width:5 ~__POS__ (he ^ "...") hello_world;
    check ~width:4 ~__POS__ (h ^ "...") hello_world;
    check ~width:3 ~__POS__ "..." hello_world;
    check ~width:2 ~__POS__ ".." hello_world;
    check ~width:1 ~__POS__ "." hello_world;
    check ~width:0 ~__POS__ "" hello_world
  in

  ()

let tests =
  Alcotest.
    [ test_case "int" `Quick test_int; test_case "string" `Quick test_string ]
