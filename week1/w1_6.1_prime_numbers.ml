let rec gcd n m =
  if m = 0 then n
  else gcd m (n mod m);;

let rec multiple_upto n r =
  if r = 1 then false
  else if n mod r = 0 then true
  else multiple_upto n (r - 1);;

let is_prime n =
  let rec aux i =
    if i < 2 then true
    else if n mod i = 0 then false
    else aux (i - 1) in
  if n < 2 then false
  else aux (integer_square_root n);;