type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf of 'a;;

let wrap l =
  List.map (fun x -> [x]) l

let rec tree_map f = function
  | Node (lt, x, rt) -> Node (tree_map f lt, f x, tree_map f rt)
  | Leaf x -> Leaf (f x)
