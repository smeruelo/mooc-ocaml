type report = message list
and message = string * status
and status = Successful | Failed

type 'a result = Ok of 'a | Error of exn


let exec f x =
  try
    Ok (f x)
  with
    e -> Error e

let compare user reference to_string =
  match user, reference with
  | (Ok u, Ok r) ->
    if u = r
    then ("got correct value " ^ to_string u, Successful)
    else ("got unexpected value " ^ to_string u, Failed)
  | (Ok u, Error _) -> ("got unexpected value " ^ to_string u, Failed)
  | (Error u, Ok _) -> ("got unexpected exception " ^ exn_to_string u, Failed)
  | (Error u, Error r) ->
    if u = r
    then ("got correct exception " ^ exn_to_string u, Successful)
    else ("got unexpected exception " ^ exn_to_string u, Failed)

let test user reference sample to_string =
  let rec aux = function
    | 0 -> []
    | i -> let s = sample () in
      (compare (exec user s) (exec reference s) to_string) :: aux (i - 1)
  in aux 10
