type report = message list
and message = string * status
and status = Successful | Failed;;

type 'a result = Ok of 'a | Error of exn;;


let exec f x =
  try
    Ok (f x)
  with
    e -> Error e;;

let compare user reference to_string =
  match user, reference with
  | (Ok u, Ok r) when u = r -> ("got correct value " ^ to_string u, Successful)
  | (Ok u, Ok _) | (Ok u, Error _) -> ("got unexpected value " ^ to_string u, Failed)
  | (Error u, Error r) when u = r -> ("got correct exception " ^ exn_to_string u, Successful)
  | (Error u, Error _) | (Error u, Ok _) -> ("got unexpected exception " ^ exn_to_string u, Failed);;

let test user reference sample to_string =
  let rec aux = function
    | 0 -> []
    | i -> let test_input = sample () in
      (compare (exec user test_input) (exec reference test_input) to_string) :: aux (i - 1)
  in aux 10;;
