open OUnit2
open Sum
open Test

let tests =
  "test suite for sum"
  >::: [ make_test "empty" 0 sum []
       ; make_test "one" 1 sum [ 1 ]
       ; make_test "onetwo" 3 sum [ 1; 2 ]
       ]

let _ = run_test_tt_main tests
