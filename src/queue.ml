module type Queue = sig
  type 'a queue
  val empty : 'a queue
  val is_empty : 'a queue -> bool
  val enqueue : 'a -> 'a queue -> 'a queue
  val peek : 'a queue -> 'a option
  val dequeue : 'a queue -> 'a queue option
end

module MyQueue : Queue = struct
  type 'a queue = [] | (::) of 'a queue * 'a
  let empty = []
  let is_empty q = q = []
  let enqueue x q = q::x
  let peek = function
    | [] -> None
    | h::t -> Some t
  let dequeue = function
  | [] -> None
  | h::t -> Some h
end
