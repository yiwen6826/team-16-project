open OUnit2

let () =
  run_test_tt_main
    ("all tests" >::: [ Test_model.suite; ])