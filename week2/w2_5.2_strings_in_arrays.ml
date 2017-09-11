let is_sorted a =
  let rec aux i =
    if i >= (Array.length a) - 1 then
      true
    else
      if String.compare a.(i) a.(i + 1) < 0 then
        aux (i + 1)
      else
        false
  in aux 0

let find dict word =
  let rec aux left right =
    let m = (left + right) / 2 in
    if m == left || m == right then
      -1
    else
      let result = String.compare word dict.(m) in
      match result with
      | -1 -> aux left m
      | 0 -> m
      | 1 -> aux m right
  in aux (-1) (Array.length dict)
