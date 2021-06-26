let () =
  let open Alcotest in
  run __FILE__ [ ("colours", Test_colours.tests); ("width", Test_width.tests) ]
