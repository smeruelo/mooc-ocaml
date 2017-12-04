exception Empty ;;

let swap ra rb =
  let tmp = !rb in
  rb := !ra;
  ra := tmp

let update r f =
  let old = !r in
  r := f !r;
  old

let move l1 l2 =
  match !l1 with
  | [] -> raise Empty
  | hd::tl -> l1 := tl; l2 := hd::!l2

let reverse l =
  let orig = ref l
  and rev = ref [] in
  try
    while true do
      move orig rev;
    done;
    !rev
  with Empty -> !rev
