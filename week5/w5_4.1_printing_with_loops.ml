let is_multiple i x = i mod x = 0

let output_multiples x n m =
  for i = n to m do
    if is_multiple i x
    then (print_string ","; print_int i)
  done

exception Custom_Zero

let display_sign_until_zero f m =
  try
    for i = 0 to m do
      match f i with
      | 0 -> raise Custom_Zero
      | x ->
        if x > 0
        then print_endline "positive"
        else print_endline "negative"
    done
  with
  | Custom_Zero -> print_endline "zero"
