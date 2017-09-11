let rec gcd n m =
  if n = m then
    n
  else if n > m then
    gcd (n - m) m
  else
    gcd n (m -n)

let multiple_upto n r =
  let rec aux i =
    if i > r then
      false
    else if n mod i = 0 then
      true
    else
      aux (i + 1)
  in aux 2

let is_prime n =
  let rec aux i =
    if i < 2 then true
    else if n mod i = 0 then false
    else aux (i - 1)
  in aux (int_of_float (sqrt (float n)))
