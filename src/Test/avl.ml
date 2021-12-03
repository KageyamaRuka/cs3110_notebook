type avl =
  | Empty
  | Node of avl * int * avl * int

let height x =
  match x with
  | Empty -> 0
  | Node (_, _, _, h) -> h

let create l v r =
  let hl = height l in
  let hr = height r in
  Node
    ( l
    , v
    , r
    , if hl >= hr then
        hl + 1
      else
        hr + 1 )

(* balance_factor = |height(l) - height(r)|, choose 2 instead 1 to avoid too many rebalance *)

let balance l v r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Empty -> failwith "impossible"
    | Node (ll, lv, lr, _) -> (
      if height ll >= height lr then
        create ll lv (create lr v r)
      else
        match lr with
        | Empty -> failwith "impossible"
        | Node (lrl, lrv, lrr, _) ->
          create (create ll lv lrl) lrv (create lrr v r))
  else if hr > hl + 2 then
    match r with
    | Empty -> failwith "impossible"
    | Node (rl, rv, rr, _) -> (
      if height rr >= height rl then
        create (create l v rl) rv rr
      else
        match rl with
        | Empty -> failwith "impossible"
        | Node (rll, rlv, rlr, _) ->
          create (create l v rll) rlv (create rlr rv rr))
  else
    Node
      ( l
      , v
      , r
      , if hl >= hr then
          hl + 1
        else
          hr + 1 )

let rec insert tree x =
  match tree with
  | Empty -> Node (Empty, x, Empty, 1)
  | Node (l, v, r, _) as t ->
    if x = v then
      t
    else if x < v then
      balance (insert l x) v r
    else
      balance l v (insert r x)

let rec mem tree x =
  match tree with
  | Empty -> false
  | Node (l, v, r, _) ->
    if v = x then
      true
    else
      mem l x || mem r x

let rec min_l_node = function
  | Empty -> failwith "impossible"
  | Node (Empty, v, _, _) -> v
  | Node (l, _, _, _) -> min_l_node l

let rec remove_min_l_node = function
  | Empty -> failwith "impossible"
  | Node (Empty, _, r, _) -> r
  | Node (l, v, r, _) -> balance (remove_min_l_node l) v r

let rec remove tree x =
  match tree with
  | Empty -> Empty
  | Node (l, v, r, _) ->
    if x = v then
      balance l (min_l_node r) (remove_min_l_node r)
    else if x > v then
      balance l v (remove r x)
    else
      balance (remove l x) v r
