#use "topfind"

#require "yojson"

open Yojson.Basic.Util

type description =
  { requires : string list
  ; text : string
  }

type exit =
  { direction : string
  ; room_id : string
  ; keys : string list
  }

type room =
  { id : string
  ; descriptions : description list
  ; point : int
  ; exits : exit list
  ; treasure : string list
  }

type item =
  { id : string
  ; description : string
  ; points : int
  }

type start_location =
  { room : string
  ; item : string
  }

type game =
  { roooms : room list
  ; start_room : string
  ; items : item list
  ; start_inv : string list
  ; start_locations : start_location list
  ; win_message : string
  }

let rooms json =
  let room j =
    let id = j |> member "id" |> to_string in
    let descriptions =
      let description =
        let requires =
          d |> member "requires" |> to_list |> List.map to_string
        in
        let text = d |> member "text" |> to_string in
        { requires; text }
      in
      j |> member "descriptions" |> to_list |> List.map description
    in
    let point = j |> member "point" |> to_string in
    let exits =
      let exit =
        let direction = es |> member "direction" |> to_string in
        let room_id = es |> member "room_id" |> to_string in
        let keys = es |> member "keys" |> to_list |> List.map to_string in
        { direction; room_id; keys }
      in
      j |> member "exits" |> to_list |> List.map exit
    in
    let treasure = j |> member "treasure" |> to_list |> List.map to_string in
    { id; descriptions; point; exits; treasure }
  in
  json |> member "rooms" |> to_list |> List.map room

let start_room json = json |> member "start_room" |> to_string

let items json =
  let item j =
    let id = j |> member "id" |> to_string in
    let description = j |> member "description" |> to_string in
    let points = j |> member "points" |> to_int in
    { id; description; points }
  in
  json |> member "items" |> to_list |> List.map item

let start_inv json = json |> member "start_inv" |> to_list |> List.map to_string

let start_locations json =
  let start_location j =
    let room = j |> member "room" |> to_string in
    let item = j |> member "item" |> to_string in
    { room; item }
  in
  json |> member "start_locations" |> to_list |> List.map start_location

let win_message json = json |> member "win_message" |> to_string
