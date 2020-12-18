let rec count_change amount coins =
  match coins with
  | [] -> 0
  | t :: n ->
    if amount = 0 then
      1
    else if amount >= t then
      count_change (amount - t) coins + count_change amount n
    else
      count_change amount n
