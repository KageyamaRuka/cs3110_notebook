let even ~number =
  if number mod 2 = 0 then
    true
  else
    false

let syr ~number =
  let rec collatz number count =
    if number = 1 then
      0
    else if even number then
      collatz (number / 2) count + 1
    else
      collatz ((number * 3) + 1) count + 1
  in
  collatz number 0
