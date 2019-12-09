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

(* Feels like cheating, but I can't get it right any other way *)
let tl cl =
  match to_list cl with
  | [] ->  None
  | hd :: tl -> Some (of_list tl);;
