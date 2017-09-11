type 'a clist =
  | CSingle of 'a
  | CApp of 'a clist * 'a clist
  | CEmpty

let example =
  CApp (CApp (CSingle 1,
              CSingle 2),
        CApp (CSingle 3,
              CApp (CSingle 4, CEmpty)))

let other_example =
  CApp (CApp (CSingle 10,
              (CApp (CSingle 20,
                     CSingle 30))),
        CApp (CSingle 40,
              CApp (CSingle 50, CEmpty)))

let to_list cl =
  let rec aux l cl =
    match cl with
    | CEmpty -> l
    | CSingle s -> s :: l
    | CApp (left, right) -> aux (aux l right) left
  in aux [] cl

let rec of_list l =
  match l with
  | [] -> CEmpty
  | hd :: tl -> CApp (CSingle hd, of_list tl)

let append cl1 cl2 =
  match (cl1, cl2) with
  | (CEmpty, cl) | (cl, CEmpty) -> cl
  | (cl1, cl2) -> CApp (cl1, cl2)

let hd cl =
  let rec aux cl =
    match cl with
    | CEmpty -> None
    | CSingle item -> Some item
    | CApp (left, right) ->
      match aux left with
      | None -> aux right
      | Some i as result -> result
  in aux cl


(* Assuming that any non-empty cl contains at least one element non-CEmty *)
let tl cl =
  let rec aux cl =
    match cl with
    | CApp (CSingle _, right) -> right
    | CApp (CEmpty, right) -> aux right
    | CApp (left, right) -> CApp (aux left, right)
  in match cl with
  | CEmpty -> None
  | CSingle item -> Some CEmpty
  | cl -> Some (aux cl)
