let filter p l =
  let f acc x =
    if p x
    then x :: acc
    else acc in
  List.fold_left f [] l;;

let partition p l =
  let f x (lpos, lneg) =
    if p x
    then (x :: lpos, lneg)
    else (lpos, x :: lneg) in
  List.fold_right f l ([], []);;

let rec sort l =
  match l with
  | [] | [_] as l -> l
  | hd :: tl -> let lpos, lneg = partition (fun x -> x <= hd) tl in
    (sort lpos) @ (hd :: (sort lneg));;
