open OUnit2
open State

let j = Yojson.Basic.from_file "threerooms.json"

let tests =
[
  "max" >:: (fun _ -> assert_equal 11111 (j |> init_state |> win_score));
]

let suite =
  "Adventure test suite"
  >::: tests

let _ = run_test_tt_main suite
