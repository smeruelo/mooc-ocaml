(*
read_lines is a **variable**, which value is a function that receives unit
that function is defined within a context in which sl exists (clousure!)
so everytime we call read_lines (), it uses the same ref (sl) to store the lines
sl is empty the first time only
*)
let read_lines =
  let sl = ref [] in
  let rec aux () =
    try
      sl := read_line () :: !sl ;
      aux ()
    with End_of_file -> List.rev !sl
  in fun () -> aux ();;


(*
read_lines_fixed is a **function**
everytime we call it, a new context is created in which sl does not exist yet
so a new empty sl is created every time
*)
let read_lines_fixed () =
  let sl = ref [] in
  let rec aux () =
    try
      sl := read_line () :: !sl ;
      aux ()
    with End_of_file -> List.rev !sl
  in aux ();
