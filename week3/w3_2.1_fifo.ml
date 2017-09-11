type queue = int list * int list

let is_empty =
  front == [] && back == []

let enqueue x (front, back) =
  (front, x :: back)

let split l =
  let med = List.length l / 2
  and len = List.length l in
  let rec aux l i front kcab =
    if i < med then
      aux (List.tl l) (i + 1) front ((List.hd l) :: kcab)
    else if i < len then
      aux (List.tl l) (i + 1) ((List.hd l) :: front) kcab
    else
      (front, List.rev kcab)
  in aux l 0 [] []

let dequeue (front, back) =
  if List.tl front == [] then
    (List.hd front, split back)
  else
    (List.hd front, (List.tl front, back))
