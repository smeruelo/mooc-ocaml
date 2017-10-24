let rec print_int_list l =
  match l with
  | [] -> ()
  | hd :: tl ->
    Printf.printf "%d\n" hd;
    print_int_list tl

let print_every_other k l =
  let rec aux i l =
    match l with
    | [] -> ()
    | hd :: tl ->
      if i mod k = 0 then
        Printf.printf "%d\n" hd;
      aux (i + 1) tl
  in aux 0 l

let rec print_list print l =
  match l with
  | [] -> ()
  | hd :: tl ->
    print hd;
    print_list print tl


(*
let rec print_int_list l =
  List.iter (fun x -> Printf.printf "%d\n" x) l

let print_every_other k l =
  List.iteri (fun i x -> if i mod k = 0 then Printf.printf "%d\n" x) l

let rec print_list print l =
  List.iter print l
*)
