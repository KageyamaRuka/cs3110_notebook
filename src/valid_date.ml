type month =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec

let leap_or_not year =
  if year mod 400 = 0 || (year mod 100 <> 0 && year mod 4 = 0) then
    true
  else
    false

let days_in_date y m =
  match m with
  | Jan
  | Mar
  | May
  | Jul
  | Aug
  | Oct
  | Dec ->
    31
  | Feb ->
    if leap_or_not y then
      29
    else
      28
  | _ -> 30

let valid_date y m d =
  if y < 1 || d < 0 || d > days_in_date y m then
    false
  else
    true
