let mem x l =
  let rec aux l =
    if l = [] then
      false
    else if List.hd l == x then
      true
    else
      aux (List.tl l)
  in aux l

let append l1 l2 =
  let rec aux l1 =
    if l1 == [] then
      l2
    else
      (List.hd l1) :: (aux (List.tl l1))
  in aux l1

let combine l1 l2 =
  let rec aux l1 l2 l =
    if l1 = [] then
      List.rev l
    else
      aux (List.tl l1) (List.tl l2) (((List.hd l1), (List.hd l2)) :: l)
  in aux l1 l2 []

let assoc l s =
  let rec aux l =
    if l == [] then
      None
    else
      let (k, x) = List.hd l in
      if String.compare k s == 0 then
        Some x
      else
        aux (List.tl l)
  in aux l
