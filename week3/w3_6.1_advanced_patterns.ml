type e = EInt of int | EMul of e * e | EAdd of e * e

let simplify = function
  | EMul (EInt 1, e) | EMul (e, EInt 1) -> e
  | EMul (EInt 0, e) | EMul (e, EInt 0) -> EInt 0
  | EAdd (EInt 0, e) | EAdd (e, EInt 0) s| e -> e

let only_small_lists = function
  | ([] | [_] | [_;_]) as l -> l
  | _ -> []

let rec no_consecutive_repetition = function
  | [] -> []
  | [x] -> [x]
  | x :: y :: ys when x = y -> no_consecutive_repetition (y :: ys)
  | x :: y :: ys -> x :: (no_consecutive_repetition (y :: ys))
