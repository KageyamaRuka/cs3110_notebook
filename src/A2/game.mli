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

val to_game : Yojson.Basic.t -> game

val get_rooms: Yojson.Basic.t -> room list

val get_start_room: Yojson.Basic.t -> string

val get_items: Yojson.Basic.t -> item list

val get_start_inv: Yojson.Basic.t -> string list

val get_win_message: Yojson.Basic.t -> string
