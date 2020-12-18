let m = 'A'

let a =
  match m with
  | 'A' .. 'Z' -> 1
  | _ -> 2

type st =
  { name : string
  ; hp : int
  ; ptype : string
  }

let get_hp m =
  let { name; hp; ptype } = m in
  hp

let get_hp2 : st -> int = fun s -> s.hp

let m = { name = "AB"; hp = 1; ptype = "BA" }

let rec list_max = function
  | [] -> None
  | h :: t -> (
    match list_max t with
    | None -> Some h
    | Some m -> Some (max h m) )

let a = (1, "a")

let b =
  match a with
  | num, _ -> num

type position = int * int

let p1 : position = (1, 2)

let p2 : int * int = (1, 2)

type point = float * float

let getx : point -> float = fun (x, _) -> x

type vector = float list

type matrix = float list list

let v1 : vector = [ 1.1; 1.2 ]

let v2 : matrix = [ [ 1.1; 1.2 ]; [ 2.; 3. ] ]

type shape =
  | Point of point
  | Circle of point * float
  | Rect of
      { left : point
      ; right : point
      }
  | A of float list

type 'a mylist =
  | Nil
  | Cons of 'a * 'a mylist

let rec length = function
  | Nil -> 0
  | Cons (_, t) -> 1 + length t

let empty = function
  | Nil -> true
  | Cons _ -> false

let c = "1234"

let f =
  try
    match c with
    | (s : string) -> failwith s
  with Failure s -> "failed"

let list_a = [ 1; 2; 3 ]

let result =
  match list_a with
  | [] -> 0
  | [ a; b; c ] -> a + b + c
  | a :: b -> a

type mon =
  { name : string
  ; hp : int
  }

let fib n =
  let rec fib_iter n a b m =
    if m = n then
      b
    else
      fib_iter n b (a + b) (m + 1)
  in
  fib_iter n 1 1 2

type student =
  { no : int
  ; name : string
  ; score : int
  }

type teacher = { name : string }

type people =
  | Teacher of teacher
  | Student of student

let tom = { no = 1; name = "tom"; score = 83 }

let a = (1, 2, 3)

let b =
  match a with
  | _, _, c -> c

let c = Some 5

type node =
  { value : int
  ; next : mylist
  }

and mylist =
  | Nil
  | Node of node

let a = { value = 1; next = Nil }

let b = { value = 2; next = Node a }

let c = Node { value = 3; next = Nil }

let d = { value = 4; next = c }
