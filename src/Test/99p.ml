(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last = function
  | [] -> None
  | [ h ] -> Some h
  | h :: t -> last t

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two = function
  | [] -> None
  | [ h ] -> None
  | [ a; b ] -> Some (a, b)
  | h :: t -> last_two t

(* 3. Find the K'th element of a list. (easy) *)
let at i =
  let rec at_iter n = function
    | [] -> None
    | h :: t ->
      if i = n then
        Some h
      else
        at_iter (n + 1) t
  in
  at_iter 1

(* 4. Find the number of elements of a list. (easy) *)
let rec length = function
  | [] -> 0
  | h :: t -> 1 + length t

(* 5. Reverse a list. (easy) *)
let reverse =
  let rec reverse_iter r = function
    | [] -> r
    | h :: t -> reverse_iter (h :: r) t
  in
  reverse_iter []

(* 6. Find out whether a list is a palindrome. (easy)

   HINT: a palindrome is its own reverse. *)
let is_palindrome l = reverse l = l
