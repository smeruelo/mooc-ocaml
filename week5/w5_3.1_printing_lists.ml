let rec print_int_list = function
  | [] -> ()
  | hd :: tl -> print_int hd; print_newline (); print_int_list tl;;

let print_every_other k l =
  let rec aux index = function
    | [] -> ()
    | hd :: tl ->
      if index mod k = 0 then (print_int hd; print_newline ());
      aux (index + 1) tl
  in aux 0 l;;

let rec print_list print l = function
  | [] -> ()
  | hd :: tl -> print hd; print_newline (); print_list print tl;;
