type index = Index of int

let read a (Index i) = a.(i)

let inside a (Index i) =
  i >= 0 && i < Array.length a

let next (Index i) = Index (i + 1)

let min_index a =
  let rec aux i mv mi =
    if inside a i then
      if read a i < mv then
        aux (next i) (read a i) i
      else
        aux (next i) mv mi
    else
      mi
  and idx0 = Index 0
  in aux idx0 (read a idx0) idx0
