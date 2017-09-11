type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt

let rec height t =
  match t with
  | Empty -> 0
  | Node (lt, n, rt) -> 1 + max (height lt) (height rt)

let rec balanced t =
  match t with
  | Empty -> true
  | Node (lt, n, rt) -> balanced lt && balanced rt && height lt = height rt
