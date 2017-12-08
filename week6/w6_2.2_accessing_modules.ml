module Tree = struct

  type 'a t =
    | Leaf of 'a
    | Node of 'a t * 'a * 'a t

  module Iterator = struct

    type 'a path =
      | Top
      | Left of 'a path * 'a * 'a t
      | Right of 'a t * 'a * 'a path

    type 'a iterator = Loc of 'a t * 'a path

    exception Fail

    let go_left (Loc (t, p)) =
      match p with
	  | Top -> raise Fail
	  | Left (father, x, right) -> raise Fail
	  | Right (left, x, father) -> Loc (left, Left (father, x, t))

    let go_right (Loc (t, p)) =
      match p with
      | Top -> raise Fail
      | Left (father, x, right) -> Loc (right, Right (t, x, father))
      | Right (left, x, father) -> raise Fail

    let go_up (Loc (t, p)) =
      match p with
	  | Top -> raise Fail
	  | Left(father, x, right) -> Loc (Node (t, x, right), father)
	  | Right(left, x, father) -> Loc (Node (left, x, t), father)

    let go_first (Loc (t, p)) =
      match t with
	  | Leaf _ -> raise Fail
	  | Node (left, x, right) -> Loc (left, Left (p, x, right))

    let go_second (Loc (t, p)) =
      match t with
	  | Leaf _ -> raise Fail
	  | Node (left, x, right) -> Loc (right, Right (left, x, p))

    let focus (Loc ((Leaf x | Node (_, x, _)), _)) = x

  end

end


let bfs t =
  let rec aux results = function
    | [] -> List.rev results
    | l :: ls ->
        let results = (Tree.Iterator.focus l) :: results in
        try
          aux results (ls @ [Tree.Iterator.go_first l; Tree.Iterator.go_second l])
        with Tree.Iterator.Fail ->
          aux results ls
  in
  aux [] [Tree.Iterator.Loc (t, Tree.Iterator.Top)]
