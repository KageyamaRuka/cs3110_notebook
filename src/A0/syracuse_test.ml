open OUnit2
open Syracuse

let make_test ~name ~expected_output ~input =
  name >:: fun _ ->
  assert_equal expected_output (syr input) ~printer:string_of_int

let tests =
  "test suite for syracuse"
  >::: [ make_test "syr1_0" 0 1
       ; make_test "syr2_1" 1 2
       ; make_test "syr10_6" 6 10
       ]

let _ = run_test_tt_main tests
