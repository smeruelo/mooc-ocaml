module Exp : sig
  type e
  val int : int -> e
  val mul : e -> e -> e
  val add : e -> e -> e
  val to_string : e -> string
end = struct

  type e = EInt of int | EMul of e * e | EAdd of e * e

  let int x = EInt x

  let mul a b =
    match a, b with
    | EInt 0, _ | _, EInt 0 -> EInt 0
    | EInt 1, e | e, EInt 1 -> e
    | a, b -> EMul (a, b)

  let add a b =
    match a, b with
    | EInt 0, e | e, EInt 0 -> e
    | a, b -> EAdd (a, b)

  let rec to_string = function
    | EInt i -> string_of_int i
    | EMul (l, r) -> "(" ^ to_string l ^ " * " ^ to_string r ^ ")"
    | EAdd (l, r) -> "(" ^ to_string l ^ " + " ^ to_string r ^ ")"

end
