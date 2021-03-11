type 'a queue =
  | []
  | ( :: ) of 'a queue * 'a

let empty = []

let is_empty q = q = []

let enqueue x q = q :: x

let peek = function
  | [] -> None
  | h :: t -> Some t

let dequeue = function
  | [] -> None
  | h :: t -> Some h
