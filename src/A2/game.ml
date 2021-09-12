let get_game_attribute attribute : Yojson.Basic.t -> Yojson.Basic.t =
  let rec get_game_attribute_iter :
      (string * Yojson.Basic.t) list -> Yojson.Basic.t = function
    | [] -> failwith "empty file"
    | h :: t ->
      match h with
      | k, v -> if k = attribute then v else get_game_attribute_iter t
  in
  function
  | `Assoc tl -> get_game_attribute_iter tl
  | _ -> failwith "wrong format"

let get_win_message json =
  let m = get_game_attribute "win_message" json in
  match m with
  | `String s -> s
  | _ -> failwith "no win_message in file"
