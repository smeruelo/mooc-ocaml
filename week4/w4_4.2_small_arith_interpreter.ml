type operation =
  | Op of string * operation * operation
  | Value of int;;

type env = (string * (int -> int -> int)) list;;

let rec lookup_function name = function
  | [] -> invalid_arg "lookup_function"
  | (str, f) :: tl when str == name -> f
  | hd :: tl -> lookup_function name tl;;

let add_function name f env =
  (name, f) :: env;;

let my_env = [
  ("min", fun x y -> if y >= x then x else y);
  ("add", fun x y -> x + y);
  ("sub", fun x y -> x - y);
  ("mul", fun x y -> x * y);
  ("div", fun x y -> x / y)
];;

let rec compute env op =
  match op with
  | Value x -> x
  | Op (f_name, op1, op2) ->
    let f = lookup_function f_name env in
    f (compute env op1) (compute env op2);;

let rec compute_eff env = function
  | Op (name, op1, op2) ->
    lookup_function name env (compute_eff env op1) (compute_eff env op2)
  | Value x -> x;;
