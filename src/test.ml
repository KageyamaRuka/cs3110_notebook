type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

let m = 'A'

let a =
  match m with
  | 'A' .. 'Z' -> 1
  | _ -> 2

type mon =
  { name : string
  ; hp : int
  ; ptype : string
  }

type st =
  { name : string
  ; hp : int
  ; ptype : string
  }

(* let get_hp m =
  let { name; hp; ptype } = m in
  hp *)

let get_hp { name; hp; ptype } = hp

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
  | Rect of {left: point;  right: point}
  | A of float list

type 'a mylist = Nil | Cons of 'a * 'a mylist
let rec length = function
| Nil -> 0
| Cons (_,t) -> 1 + length t

let empty = function
  | Nil -> true
  | Cons _ -> false