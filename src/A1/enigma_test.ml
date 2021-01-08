open OUnit2
open Enigma

(*******************************************************************)
(* Helper values used throughout this test suite. *)
(*******************************************************************)

let rotor_id_wiring = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let rotor_I_wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"

let rotor_II_wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE"

let rotor_III_wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO"

let refl_B_wiring = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

(*******************************************************************)
(* Tests for Part 1 *)
(*******************************************************************)

let index_tests =
  [ ("index A" >:: fun _ -> assert_equal 0 (index 'A'))
  ; ("index Z" >:: fun _ -> assert_equal 25 (index 'Z'))
  ]

(* You do not need to construct a test sweep for [index].  
 * It's too simple of a function for that to be worthwhile. *)

(*******************************************************************)
(* Tests for Part 2 *)
(*******************************************************************)

(* README: In the test case list below, the sweep cases are the first five in
 * the list.  We ask you to follow that format as you complete the
 * rest of the test suite for all the other functions.  You are free
 * to move any provided test cases into the sweep, as long as you 
 * document why the test case is interesting. *)

let map_rl_tests =
  [ (* Sweep case 1:  this is interesting because it tests a rotor whose wiring
     *   is as simple as possible:  every contact on the LHS connects directly
     *   to the corresponding contact on the RHS. *)
    ( "rl_id0" >:: fun _ ->
      assert_equal 4 (map_r_to_l "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0) )
  ; (* Sweep case 2: TODO *)
    ( "rl_id1" >:: fun _ ->
      assert_equal 1 (map_r_to_l "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'A' 0) )
  ; (* Sweep case 3: TODO *)
    ( "rl_id2" >:: fun _ ->
      assert_equal 25 (map_r_to_l "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'B' 0) )
  ; (* Sweep case 4: TODO *)
    ( "rl_id3" >:: fun _ ->
      assert_equal 0 (map_r_to_l "BACDEFGHIJKLMNOPQRSTUVWXYZ" 'C' 0) )
  ; (* Sweep case 5: TODO *)
    ( "rl_id4" >:: fun _ ->
      assert_equal 9 (map_r_to_l "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0) )
  ; (* Other test cases (not part of the sweep) *)
    ("rl_ex1" >:: fun _ -> assert_equal 4 (map_r_to_l rotor_I_wiring 'A' 0))
  ; ("rl_ex2" >:: fun _ -> assert_equal 9 (map_r_to_l rotor_I_wiring 'B' 0))
  ]

let map_lr_tests =
  [ (* TODO: test sweep *)

    (* Other test cases (not part of the sweep) *)
    ("lr_ex3" >:: fun _ -> assert_equal 20 (map_l_to_r rotor_I_wiring 'A' 0))
  ]

(*******************************************************************)
(* Tests for Part 3 *)
(*******************************************************************)

let map_refl_tests =
  [ (* TODO: test sweep *)

    (* Other test cases (not part of the sweep) *)
    ("refl_B0" >:: fun _ -> assert_equal 24 (map_refl refl_B_wiring 0))
  ]

(*******************************************************************)
(* Tests for Part 4 *)
(*******************************************************************)

let map_plug_tests =
  [ (* TODO: test sweep *)

    (* Other test cases (not part of the sweep) *)
    ("plug_empty" >:: fun _ -> assert_equal 'A' (map_plug [] 'A'))
  ]

(*******************************************************************)
(* Tests for Part 5 *)
(*******************************************************************)

let id_config =
  { refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"; rotors = []; plugboard = [] }

let rotor_I = { wiring = rotor_I_wiring; turnover = 'Q' }

let rotor_II = { wiring = rotor_II_wiring; turnover = 'E' }

let rotor_III = { wiring = rotor_III_wiring; turnover = 'V' }

let cipher_char_ex_config =
  { refl = refl_B_wiring
  ; rotors =
      [ { rotor = rotor_I; top_letter = 'A' }
      ; { rotor = rotor_II; top_letter = 'A' }
      ; { rotor = rotor_III; top_letter = 'A' }
      ]
  ; plugboard = []
  }

let cipher_char_tests =
  [ (* TODO: test sweep *)

    (* Other test cases (not part of the sweep) *)
    ("cipher_id" >:: fun _ -> assert_equal 'A' (cipher_char id_config 'A'))
  ; ( "cipher_ex" >:: fun _ ->
      assert_equal 'P' (cipher_char cipher_char_ex_config 'G') )
  ]

(*******************************************************************)
(* Tests for Part 6 *)
(*******************************************************************)

let step_ex_config =
  { refl = refl_B_wiring
  ; rotors =
      [ { rotor = rotor_III; top_letter = 'K' }
      ; { rotor = rotor_II; top_letter = 'D' }
      ; { rotor = rotor_I; top_letter = 'O' }
      ]
  ; plugboard = []
  }

let step_ex_config' =
  { refl = refl_B_wiring
  ; rotors =
      [ { rotor = rotor_III; top_letter = 'K' }
      ; { rotor = rotor_II; top_letter = 'D' }
      ; { rotor = rotor_I; top_letter = 'P' }
      ]
  ; plugboard = []
  }

let step_tests =
  [ (* TODO: test sweep *)

    (* Other test cases (not part of the sweep) *)
    ("step_ex1a" >:: fun _ -> assert_equal step_ex_config' (step step_ex_config))
  ]

(*******************************************************************)
(* Tests for Part 7 *)
(*******************************************************************)

let cipher_ex_config =
  { refl = refl_B_wiring
  ; rotors =
      [ { rotor = rotor_I; top_letter = 'F' }
      ; { rotor = rotor_II; top_letter = 'U' }
      ; { rotor = rotor_III; top_letter = 'N' }
      ]
  ; plugboard = [ ('A', 'Z') ]
  }

let cipher_tests =
  [ (* TODO: test sweep *)

    (* Other test cases (not part of the sweep) *)
    ("ex" >:: fun _ -> assert_equal "OCAML" (cipher cipher_ex_config "YNGXQ"))
  ]

let tests =
  "test suite for A1"
  >::: List.flatten
         [ index_tests
         ; map_rl_tests
         ; map_lr_tests
         ; map_refl_tests
         ; map_plug_tests
         ; cipher_char_tests
         ; step_tests
         ; cipher_tests
         ]

let _ = run_test_tt_main tests
