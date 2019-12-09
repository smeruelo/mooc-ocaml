type 'a clist =
  | CSingle of 'a
  | CApp of 'a clist * 'a clist
  | CEmpty;;

let to_list cl =
  let rec aux l = function
    | CSingle cs -> cs :: l
    | CApp (cl_left, cl_right) -> aux (aux l cl_right) cl_left
    | CEmpty -> l
  in aux [] cl;;

let rec of_list = function
  | [] -> CEmpty
  | hd :: tl -> CApp (CSingle hd, of_list tl);;

let append cl1 cl2 =
  match cl1, cl2 with
  | CEmpty, cl | cl, CEmpty -> cl
  | cl1, cl2 -> CApp (cl1, cl2);;

let hd cl =
  let rec aux = function
    | CEmpty -> None
    | CSingle cs -> Some cs
    | CApp (cl_left, cl_right) -> match aux cl_left with
      | None -> aux cl_right
      | Some cs -> Some cs
  in aux cl;;

let tl cl =
  let rec aux = function
    | CEmpty -> (CEmpty, false)
    | CSingle _ -> (CEmpty, true)
    | CApp (left, right) -> match aux left with
      | left', false -> let (right', found) = aux right in (CApp (left', right'), found)
      | left', true -> (CApp (left', right), true)
  in match aux cl with
  | _, false -> None
  | cl_tl, true -> Some cl_tl;;
