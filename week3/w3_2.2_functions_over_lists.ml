let rec mem x = function
  | [] -> false
  | hd :: tl -> if hd = x then true else mem x tl;;

let rec append l1 l2 =
  match l1 with
  | [] -> l2
  | hd :: tl -> hd :: append tl l2;;

let rec combine l1 l2 =
  match l1, l2 with
  | ([], []) -> []
  | (hd1 :: tl1, hd2 :: tl2) -> (hd1, hd2) :: combine tl1 tl2;;

let rec assoc l k =
  match l with
  | [] -> None
  | (key, value) :: tl -> if key = k then Some value else assoc tl k;;
