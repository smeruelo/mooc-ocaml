module type GenericTrie = sig
  type 'a char_table
  type 'a trie = Trie of 'a option * 'a trie char_table
  val empty : unit -> 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val lookup : 'a trie -> string -> 'a option
end

module CharHashedType = struct
  type t = char
  let equal c1 c2 = c1 = c2
  let hash c = Char.code c
end;;

module CharHashtbl = Hashtbl.Make(CharHashedType);;

module Trie : GenericTrie
  with type 'a char_table = 'a CharHashtbl.t =
struct
  type 'a char_table = 'a CharHashtbl.t
  type 'a trie = Trie of 'a option * 'a trie char_table

  let empty () = Trie (None, CharHashtbl.create 100)

  let lookup trie w =
    let rec aux i (Trie (value, children)) =
      if i >= String.length w then
        value
      else
        try
          aux (i + 1) (CharHashtbl.find children w.[i])
        with Not_found -> None
    in aux 0 trie

  let insert trie w v =
    let rec aux i (Trie (value, children)) =
      if i >= String.length w then
        Trie (Some v, children)
      else
        try
          let subtrie = CharHashtbl.find children w.[i] in
          let newtrie = aux (i + 1) subtrie in
          CharHashtbl.replace children w.[i] newtrie ;
          Trie (value, children)
        with Not_found ->
          begin
            CharHashtbl.add children w.[i] (aux (i + 1) (empty ()));
            Trie (value, children)
          end
    in aux 0 trie
end;;
