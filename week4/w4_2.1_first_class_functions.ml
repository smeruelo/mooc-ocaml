type int_ff = int -> int

let rec compose = function
  | [] -> fun x -> x
  | f :: tl -> fun x -> f (compose tl x)

let rec fixedpoint f start delta =
  let current = f start in
  let next = f current in
  if abs_float (current -. next) < delta
  then start
  else fixedpoint f current delta
