(* let list_max li =
  let rec list_max_iter l i =
    match l with
    | [] -> i
    | h :: t -> list_max_iter t (max (Some h) i)
  in
  list_max_iter li None *)

(* module M : sig
  type (+'a, +'b) t
end = struct
  type ('a, 'b) t = 'a * 'b
end

let f x = (x : ([ `A ], [ `B ]) M.t :> ([ `A | `C ], [ `B | `D ]) M.t) *)

(* module M : sig
  type (-'a, +'b) t
end = struct
  type ('a, 'b) t = 'a -> 'b
end

let f x = (x : ([ `A | `B ], [ `C ]) M.t :> ([ `A ], [ `C | `D ]) M.t) *)
let b = "bigred"
let inc x = x+1
module M = struct
  let y = 42
end