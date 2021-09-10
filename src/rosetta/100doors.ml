type status =
  | Open
  | Closed

type door =
  { door_status : status
  ; index : int
  }

let gen_door_row num =
  let rec gen_door_row_iter num i doors : door list =
    if num = i then
      doors
    else
      gen_door_row_iter num (i + 1) ({ door_status = Closed; index = i } :: doors)
  in
  gen_door_row_iter (num + 1) 1 []

let toggle_door d =
  match d.door_status with
  | Open -> { d with door_status = Closed }
  | Closed -> { d with door_status = Open }

let rec toggle_doors doors step =
  match doors with
  | [] -> []
  | h :: t ->
    if h.index mod step = 0 then
      toggle_door h :: toggle_doors t step
    else
      h :: toggle_doors t step

let doors = gen_door_row 100

let rec toggle_doors_iter doors start stop =
  if start = stop then
    doors
  else
    toggle_doors_iter (toggle_doors doors start) (start + 1) stop

let doors_100 = toggle_doors_iter (gen_door_row 100) 1 101