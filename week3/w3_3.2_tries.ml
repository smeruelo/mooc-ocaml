type trie = Trie of int option * char_to_children
and char_to_children = (char * trie) list

let empty = Trie (None, [])

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

let children_from_char c2c c =
  let rec aux = function
    | [] -> None
    | (c', t) :: tl when c' = c -> Some t
    | _ :: tl -> aux tl
  in aux c2c

let update_children c2c c t =
  let rec aux = function
    | [] -> [(c, t)]
    | (c', t') :: tl when c' = c -> (c, t) :: tl
    | hd :: tl -> hd :: (aux tl)
  in aux c2c

let lookup t key =
  let rec aux i (Trie (value, c2c)) =
    if i >= String.length key
    then value
    else match children_from_char c2c key.[i] with
      | None -> None
      | Some t -> aux (i + 1) t
  in aux 0 t

let insert t key value =
  let rec aux i (Trie (v, c2c)) =
    if i >= String.length key
    then Trie (Some value, c2c)
    else match children_from_char c2c key.[i] with
      | None -> Trie (v, update_children c2c key.[i] (aux (i + 1) empty))
      | Some t -> Trie (v, update_children c2c key.[i] (aux (i + 1) t))
  in aux 0 t
