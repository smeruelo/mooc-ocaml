let filter p l =
  List.fold_right (fun x xs -> if p x then x::xs else xs) l []

(* assumed that:
  l is 'a list
  f is (a' -> a' -> 'a)
*)
let reduce f neutral l =
  List.fold_left f neutral l

(* assumed that:
   l is a' list
   f is (a' -> 'b)
*)
let map f l =
  List.fold_right (fun x xs -> (f x)::xs) l []

let partition p l =
  let f = fun x (xs_pos, xs_neg) ->
    if p x
    then (x::xs_pos, xs_neg)
    else (xs_pos, x::xs_neg) in
  List.fold_right f l ([], [])

let rec sort = function
  | [] | [_] as l -> l
  | h::r ->
    let (lessers, greaters) = (partition (fun x -> x < h) r) in
    (sort lessers) @ (h :: (sort greaters))
