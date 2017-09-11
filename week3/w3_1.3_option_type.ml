let find (a : string array) (w : string) =
  let rec aux i =
    if i < Array.length a then
      if a.(i) == w then
        Some i
      else
        aux (i + 1)
    else
      None
  in aux 0

let default_int x =
  match x with
  | None -> 0
  | Some x -> x

let merge a b =
  match (a, b) with
  | (None, None) -> None
  | (None, Some x) | (Some x, None) -> Some x
  | (Some x, Some y) -> Some (x + y)
