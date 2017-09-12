type operation =
  | Op of string * operation * operation
  | Value of int

type env = (string * (int -> int -> int)) list

let rec lookup_function s = function
  | [] -> (invalid_arg "lookup_function")
  | (hd_s, hd_f) :: tl ->
    if hd_s = s
    then hd_f
    else lookup_function s tl

let add_function name f env =
  (name, f) :: env

let my_env =
  [("min", fun x y -> if x <= y then x else y)]

let rec compute env op =
  match op with
  | Value x -> x
  | Op (f_name, op1, op2) ->
    let f = lookup_function f_name env
    in f (compute env op1) (compute env op2)

(* su abuela sabrá que hacer en este apartado *)
let rec compute_eff env = function _ ->
  "Replace this string with your implementation"
