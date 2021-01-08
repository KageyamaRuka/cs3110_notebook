(*
 * CS 3110 Fall 2017 A1
 * Author:
 * NetID:
 *
 * Acknowledge here any contributions made to your solution that
 * did not originate from you or from the course staff:
 *
 *)

(*********************************************************)
(* PART 1 *)
(*********************************************************)

(* [index c] is the 0-based index of [c] in the alphabet.
 * requires: c is an uppercase letter in A..Z *)
let index c =
  match c with
  | 'A' -> 0
  | 'B' -> 1
  | 'C' -> 2
  | 'D' -> 3
  | 'E' -> 4
  | 'F' -> 5
  | 'G' -> 6
  | 'H' -> 7
  | 'I' -> 8
  | 'J' -> 9
  | 'K' -> 10
  | 'L' -> 11
  | 'M' -> 12
  | 'N' -> 13
  | 'O' -> 14
  | 'P' -> 15
  | 'Q' -> 16
  | 'R' -> 17
  | 'S' -> 18
  | 'T' -> 19
  | 'U' -> 20
  | 'V' -> 21
  | 'W' -> 22
  | 'X' -> 23
  | 'Y' -> 24
  | 'Z' -> 25
  | _ -> failwith "c is an uppercase letter in A..Z"

let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let index_reverse wiring n = String.index wiring alphabet.[n]

(* NOTE:  [failwith "Unimplemented"] raises an exception to indicate
   that the function has not been finished.  You should delete that
   line of code and replace it with your own code. *)

(*********************************************************)
(* PART 2 *)
(*********************************************************)

(* [map_r_to_l wiring top_letter input_pos] is the left-hand output position
 * at which current would appear when current enters at right-hand input
 * position [input_pos] to a rotor whose wiring specification is given by
 * [wiring].  The orientation of the rotor is given by [top_letter], 
 * which is the top letter appearing to the operator in the rotor's 
 * present orientation.
 * requires: 
 *  - [wiring] is a valid wiring specification.
 *  - [top_letter] is in 'A'..'Z'
 *  - [input_pos] is in 0..25
 *)
(* ("rl_id0" >:: fun _ -> assert_equal 0 (map_r_to_l rotor_id_wiring 'A' 0)) *)
let map_r_to_l wiring top_letter input_pos =
  let offset = index top_letter in
  (index wiring.[(input_pos + offset) mod 26] - offset + 26) mod 26

(* [map_l_to_r] computes the same function as [map_r_to_l], except
 * for current flowing left to right. *)
let map_l_to_r wiring top_letter input_pos =
  let offset = index top_letter in
  (index_reverse wiring ((input_pos + offset) mod 26) - offset + 26) mod 26

(*********************************************************)
(* PART 3 *)
(*********************************************************)

(* [map_refl wiring input_pos] is the output position at which current would 
 * appear when current enters at input position [input_pos] to a reflector 
 * whose wiring specification is given by [wiring].
 * requires: 
 *  - [wiring] is a valid reflector specification.
 *  - [input_pos] is in 0..25
 *)
let map_refl wiring input_pos = failwith "Unimplemented"

(*********************************************************)
(* PART 4 *)
(*********************************************************)

(* [map_plug plugs c] is the letter to which [c] is transformed
 * by the plugboard [plugs].
 * requires:
 *  - [plugs] is a valid plugboard
 *  - [c] is in 'A'..'Z' 
 *)
let rec map_plug plugs c = failwith "Unimplemented"

(*********************************************************)
(* PART 5 *)
(*********************************************************)

type rotor =
  { wiring : string
  ; turnover : char
  }

type oriented_rotor =
  { rotor : rotor
  ; top_letter : char
  }

type config =
  { refl : string
  ; rotors : oriented_rotor list
  ; plugboard : (char * char) list
  }

(* [cipher_char config c] is the letter to which the Enigma machine 
 * ciphers input [c] when it is in configuration [config].
 * requires:
 *  - [config] is a valid configuration
 *  - [c] is in 'A'..'Z'
 *)
let cipher_char config c = failwith "Unimplemented"

(*********************************************************)
(* PART 6 *)
(*********************************************************)

(* [step config] is the new configuration to which the Enigma machine 
 * transitions when it steps beginning in configuration [config].
 * requires: [config] is a valid configuration
 *)
let step config = failwith "Unimplemented"

(*********************************************************)
(* PART 7 *)
(*********************************************************)

(* [cipher config s] is the string to which [s] enciphers
 * when the Enigma machine begins in configuration [config].
 * requires: 
 *   - [config] is a valid configuration 
 *   - [s] contains only uppercase letters
 *)
let rec cipher config s = failwith "Unimplemented"
