type queue = int list * int list;;

let is_empty (front, back) =
  front == [] && back == [];;

let enqueue x (front, back) =
  (front, x :: back);;

let split l =
  let rec aux back lst =
    match lst with
    | lst when List.length lst - List.length back <= 1 -> (List.rev lst, List.rev back)
    | hd :: tl -> aux (hd :: back) tl in
  aux [] l;;

let dequeue (front, back) =
  match front with
  | [] -> let (f_hd :: f_tl), b = split back in (f_hd, (f_tl, b))
  | hd :: tl -> (hd, (tl, back));;
