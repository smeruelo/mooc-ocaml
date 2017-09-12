let rec last_element = function
  | [] -> (invalid_arg "last_element")
  | x :: [] -> x
  | x :: tl -> last_element tl

let rec is_sorted = function
  | [_] | [] -> true
  | x :: y :: tl -> x < y && is_sorted y :: tl
