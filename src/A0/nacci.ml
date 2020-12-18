let rec append l a =
  match l with
  | [] -> [ a ]
  | i :: t -> i :: append t a

let rec sum stack =
  match stack with
  | [] -> 0
  | i :: t -> i + sum t

let rec len stack =
  match stack with
  | [] -> 0
  | i :: t -> 1 + len t

(* 
let rec range num step start =
  if num < 1 then
    [ start ]
  else
    start :: range (num - 1) step (start + step) *)

let update_stack stack =
  match stack with
  | [] -> []
  | e :: list -> append list (sum stack)

let gen_stack n =
  let rec gen_stack_iter n stack x =
    if x = n then
      stack
    else
      gen_stack_iter n (append stack (sum stack)) (x + 1)
  in
  match n with
  | 0 -> []
  | 1 -> [ 1 ]
  | n -> gen_stack_iter n [ 1 ] 1

let nacci n k =
  let rec nacci_iter n k stack j result =
    if j = k then
      result
    else
      nacci_iter n k (update_stack stack) (j + 1) (sum stack)
  in
  if k < n then
    sum (gen_stack (k - 1))
  else
    nacci_iter n k (gen_stack n) n (sum (gen_stack (n - 1)))

let fib n =
  let rec fib_iter n f1 f2 m =
    if n < 3 then
      1
    else if m = n then
      f2
    else
      fib_iter n f2 (f1 + f2) (m + 1)
  in
  fib_iter n 1 1 2
