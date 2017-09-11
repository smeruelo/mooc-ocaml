type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp

let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3))

let my_example =
  EAdd (EMul (EInt 2, EInt 2), EMul (EInt 3, EInt 3))

let rec eval e =
  match e with
  | EInt i -> i
  | EAdd (e1, e2) -> eval e1 + eval e2
  | EMul (e1, e2) -> eval e1 * eval e2

let factorize e =
  match e with
  | EAdd (EMul (a, b), (EMul (c, d))) ->
    if a = c then
      EMul (a, EAdd (b, d))
    else if a = d then
      EMul (a, EAdd (b, c))
    else if b = c then
      EMul (b, EAdd (a, d))
    else if b = d then
      EMul (b, EAdd (a, c))
    else
      e
  | _ -> e

let expand e =
  match e with
  | EMul (a, EAdd (b, c)) -> EAdd (EMul (a, b), (EMul (a, c)))
  | _ -> e

(* rewritings are recursive! *)
let simplify e =
  match e with
  | EMul (_, (EInt 0)) -> EInt 0
  | EMul ((EInt 0), _) -> EInt 0
  | EMul (a, (EInt 1)) -> a
  | EMul ((EInt 1), b) -> b
  | EAdd (a, (EInt 0)) -> a
  | EAdd ((EInt 0), b) -> b
  | EMul (a, b) -> simplify (EMul (simplify a, simplify b))
  | EAdd (a, b) -> simplify (EAdd (simplify a, simplify b))
  | _ -> e
