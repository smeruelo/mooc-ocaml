type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty =
  Trie (None, [])

let example =
  Trie (None,
	    [('i', Trie (Some 11,
                     [('n', Trie (Some 5, [('n', Trie (Some 9, []))]))]));
	     ('t',
	      Trie (None,
		        [('e',
		          Trie (None,
			            [('n', Trie (Some 12, [])); ('d', Trie (Some 4, []));
			             ('a', Trie (Some 3, []))]));
		         ('o', Trie (Some 7, []))]));
	     ('A', Trie (Some 15, []))])

let children_from_char m c =
  let rec aux l =
    match l with
    | [] -> None
    | (c', t)::tl ->
      if c = c'
      then Some t
      else aux tl
  in aux m

let update_children m c t =
  let rec aux m =
    match m with
    | [] -> [(c, t)]
    | (c', t')::tl ->
      if c = c'
      then (c, t)::tl
      else (c', t')::(aux tl)
  in aux m

let lookup t s =
  let rec aux idx (Trie (i, children)) =
    if idx >= String.length s
    then i
    else match children_from_char children s.[idx] with
      | None -> None
      | Some t -> aux (idx + 1) t
  in aux 0 t

let insert t s k =
  let rec aux idx (Trie (i, children)) =
    if idx >= String.length s
    then Trie (Some k, children)
    else match children_from_char children s.[idx] with
      | None -> Trie (i, update_children children s.[idx] (aux (idx + 1) empty))
      | Some t -> Trie (i, update_children children s.[idx] (aux (idx + 1) t))
  in aux 0 t
