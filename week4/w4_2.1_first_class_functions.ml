type int_ff = int -> int

let rec compose = function
  | [] -> fun x -> x
  | f :: tl -> fun x -> f (compose tl x)

let rec fixedpoint f start delta =
  let next = f start in
  if abs_float (next -. start) < delta
  then start
  else fixedpoint f next delta
