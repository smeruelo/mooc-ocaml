type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt;;

let rec height = function
  | Empty -> 0
  | Node (lt, _, rt) -> 1 + max (height lt) (height rt);;

let rec balanced = function
  | Empty -> true
  | Node (lt, _, rt) -> balanced lt && balanced rt && height lt = height rt;;
