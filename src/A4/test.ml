open OUnit2
open Ast
open Main

let str_max_int = string_of_int max_int
let str_min_int = string_of_int min_int

(* OCaml allows {|...|} as a syntax for strings in which the ...
   can contain unescaped quotes.  This is super useful for
   constructing test cases, as shown below. *)
   
(* all of these tests will currently fail, because you have not
   yet implemented interpretation of any of these syntactic forms *)
let tests = [
  {|42|}, "42";
  {|-1|}, "-1";
  str_max_int, str_max_int;
  str_min_int, str_min_int;
  {|true|}, "true";
  {|false|}, "false";
  {|undefined|}, "undefined";
  {|"xyzzy"|}, {|"xyzzy"|};
  {|4/0|}, {|Exception: "Division by zero"|};
  {|4 mod 0|}, {|Exception: "Division by zero"|};
  {|let x = 0 in y|}, {|Exception: "Unbound variable"|};
  {|throw 0|}, "Exception: 0";
  {|fun (x) -> 0|}, "<closure>";
  {|0 0|}, {|Exception: "Application: not a function"|};
  {|(fun (x) -> 0) 1 2|}, {|Exception: "Application: wrong number of arguments"|};
  {|ref 0|}, "<location>";
  {|1 := 0|}, {|Exception: "Assignment to non-location"|};
  {|{"x":1}|}, "<object>";
]

let make_interp_expr_test idx in_str out_str =
  "test" ^ (string_of_int idx) >:: (fun _ -> assert_equal out_str (interp_expr in_str))

let _ = run_test_tt_main ("suite" >::: 
  List.mapi (fun idx (i, o) -> make_interp_expr_test idx i o) tests)
