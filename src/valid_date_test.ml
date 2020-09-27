open OUnit2
open Valid_date

let make_test ~name ~expected_output ~year ~month ~day =
  name >:: fun _ ->
  assert_equal expected_output
    (valid_date year month day)
    ~printer:string_of_bool

let tests =
  "test suite for valid_date"
  >::: [ make_test "valid_20201001" true 2020 Oct 1
       ; make_test "invalid_20201032" false 2020 Oct 32
       ; make_test "valid_20200229" true 2020 Feb 29
       ; make_test "invalid_20190229" false 2019 Feb 29
       ; make_test "invalid_21000229" false 2100 Feb 29
       ; make_test "valid_20000229" true 2000 Feb 29
       ; make_test "invalid_20000200" false 2000 Feb 0
       ]

let _ = run_test_tt_main tests
