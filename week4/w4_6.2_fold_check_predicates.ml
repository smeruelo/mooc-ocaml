let for_all p l =
  List.fold_left (fun acc x -> acc && p x) true l;;

let exists p l =
  List.fold_left (fun acc x -> acc || p x) false l;;

let sorted cmp = function
  | [] -> true
  | hd :: tl as l ->
    let f acc x =
      match acc with
      | Some y -> if cmp y x <= 0 then Some x else None
      | None -> None in
    match List.fold_left f (Some (List.hd l)) l with
    | Some _ -> true
    | None -> false;;
