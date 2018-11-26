type exp =
  | EInt of int
  | EAdd of exp * exp
  | EMul of exp * exp;;

let example =
  EAdd (EInt 1, EMul (EInt 2, EInt 3));;

let my_example = 
  EAdd (EMul (EInt 2, EInt 2), EMul (EInt 3, EInt 3));;

let rec eval e =
  match e with
  | EInt x ->  x
  | EAdd (e1, e2) -> eval e1 + eval e2
  | EMul (e1, e2) -> eval e1 * eval e2;;

let factorize e =
  match e with
  | EAdd (EMul (e1, e2), EMul (e3, e4)) when e1 = e3 -> EMul (e1, EAdd (e2, e4))
  | _ -> e;;

let expand e =
  match e with
  | EMul (e1, EAdd (e2, e3)) -> EAdd (EMul (e1, e2), EMul (e1, e3))
  | _ -> e;;

let simplify e =
  match e with
  | EMul (e1, e2) when e1 = EInt 0 || e2 = EInt 0 -> EInt 0
  | EMul (e, EInt 1) -> e
  | EMul (EInt 1, e) -> e
  | EAdd (e, EInt 0) -> e
  | EAdd (EInt 0, e) -> e
  | _ -> e;;
