module type MultiSet_S = sig

  (* A multi-set of type ['a t] is a collection of values of
     type ['a] that may occur several times. *)
  type 'a t

  (* [occurrences s x] return the number of time [x] occurs
     in [s]. *)
  val occurrences : 'a t -> 'a -> int

  (* The empty set has no element. There is only one unique
     representation of the empty set. *)
  val empty : 'a t

  (* [insert s x] returns a new multi-set that contains all
     elements of [s] and a new occurrence of [x]. Typically,
     [occurrences s x = occurrences (insert s x) x + 1]. *)
  val insert : 'a t -> 'a -> 'a t

  (* [remove s x] returns a new multi-set that contains all elements
     of [s] minus an occurrence of [x] (if [x] actually occurs in
     [s]). Typically, [occurrences s x = occurrences (remove s x) x -
     1] if [occurrences s x > 0]. *)
  val remove : 'a t -> 'a -> 'a t

end


module MultiSet : MultiSet_S = struct
  type 'a t = ('a * int) list

  let occurrences s x =
    try
      List.assoc x s
    with Not_found -> 0

  let empty = []

  let insert s x =
    match occurrences s x with
    | 0 -> List.sort compare ((x, 1) :: s)
    | i -> List.sort compare ((x, (i + 1)) :: (List.remove_assoc x s))

  let remove s x =
    match occurrences s x with
    | 0 -> s
    | 1 -> List.remove_assoc x s
    | i -> List.sort compare ((x, (i - 1)) :: (List.remove_assoc x s))

end ;;


let letters word =
  let rec aux s = function
    | 0 -> s
    | i -> aux (MultiSet.insert s word.[i - 1]) (i - 1)
  in aux MultiSet.empty (String.length word);;

let anagram word1 word2 =
  letters word1 = letters word2 ;;
