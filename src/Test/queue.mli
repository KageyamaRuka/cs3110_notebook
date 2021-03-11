type 'a queue

val empty : 'a queue

val is_empty : 'a queue -> bool

val enqueue : 'a -> 'a queue -> 'a queue

val peek : 'a queue -> 'a option

val dequeue : 'a queue -> 'a queue option
