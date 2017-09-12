let rec equal_on_common = function
  | [] -> fun l2 -> true
  | h1 :: t1 -> function
    | [] -> true
    | h2 :: t2 -> h1 = h2 && equal_on_common t1 t2
