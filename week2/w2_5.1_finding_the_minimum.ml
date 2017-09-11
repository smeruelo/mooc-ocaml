let min a =
  let rec aux i m =
    if i < Array.length a then
      if a.(i) < m then
        aux (i + 1) a.(i)
      else
        aux (i + 1) m
    else
      m
  in aux 0 a.(0)

let min_index a =
  let rec aux i mv mi =
    if i < Array.length a then
      if a.(i) < mv then
        aux (i + 1) a.(i) i
      else
        aux (i + 1) mv mi
    else
      mi
  in aux 0 a.(0) 0

let it_scales = "yes"
