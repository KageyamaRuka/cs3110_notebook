open OUnit2

let make_test ~name ~expected_output ~f ~input =
  name >:: fun _ ->
  assert_equal expected_output (f input) ~printer:string_of_int

type point =
  { x : int
  ; y : int
  }

let x = { x = 3; y = 4 }

;;
match x with
| { y = v } -> v

let y = { x with y = 5 }

;;
print_int y.y
