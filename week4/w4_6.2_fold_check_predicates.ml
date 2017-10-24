let for_all p l =
  List.fold_left (fun a h -> a && (p h)) true l

let exists p l =
  List.fold_left (fun a h -> a || (p h)) false l

let sorted cmp l =
  let f a h =
    match a with
    | Some a -> if cmp a h <= 0 then Some h else None
    | None -> None
  in
  match List.fold_left f (Some (List.hd l)) l with
  | Some _ -> true
  | None -> false

let my_cmp a b =
  if a > b then 1
  else if b > a then -1
  else 0
