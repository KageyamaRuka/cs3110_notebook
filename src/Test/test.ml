type color =
  | Red
  | Black

type 'a rbtree =
  | Node of color * 'a * 'a rbtree * 'a rbtree
  | Leaf

let a = Node (Red, 1, Leaf, Leaf)