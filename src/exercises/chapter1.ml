(* Exercise: values [✭]

What is the type and value of each of the following OCaml expressions?

    7 * (1+2+3)
    "CS " ^ string_of_int 3110

Hint: type each expression into the toplevel and it will tell you the answer. Note: ^ is not exponentiation. *)

;;
7 * (1 + 2 + 3)

;;
"CS " ^ string_of_int 3110

(* Exercise: operators [✭✭]

Examine the table of all operators in the OCaml manual (you will have to scroll down to find it on that page).

    Write an expression that multiplies 42 by 10.
    Write an expression that divides 3.14 by 2.0. Hint: integer and floating-point operators are written differently in OCaml.
    Write an expression that computes 4.2 raised to the seventh power. Note: there is no built-in integer exponentiation operator in OCaml (nor is there in C, by the way), in part because it is not an operation provided by most CPUs.
 *)

;;
42 * 10

;;
3.14 /. 2.0

;;
4.2 ** 7.

(* Exercise: equality [✭]

    Write an expression that compares 42 to 42 using structural equality.
    Write an expression that compares "hi" to "hi" using structural equality. What is the result?
    Write an expression that compares "hi" to "hi" using physical equality. What is the result? *)

;;
42 = 42

;;
"hi" = "hi"

;;
"hi" == "hi"

(* Exercise: assert [✭]

    Enter assert true;; into utop and see what happens.
    Enter assert false;; into utop and see what happens.
    Write an expression that asserts 2110 is not (structurally) equal to 3110. *)

;;
assert true

;;
assert false

;;
assert (2110 <> 3110)
