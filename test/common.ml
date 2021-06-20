let check_invalid ~__POS__:pos f =
  match f () with
  | _ ->
      Alcotest.fail ~pos
        "Expected [Invalid_argument], but no exception was raised."
  | exception Invalid_argument _ -> ()
