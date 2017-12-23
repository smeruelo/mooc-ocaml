module type DictSig = sig
  type ('key, 'value) t
  val empty : ('key, 'value) t
  val add : ('key, 'value) t -> 'key -> 'value -> ('key, 'value) t
  exception NotFound
  val lookup : ('key, 'value) t -> 'key -> 'value
  val remove : ('key, 'value) t -> 'key -> ('key, 'value) t
end ;;


module Dict : DictSig = struct

  type ('key, 'value) t =
    | Empty
    | Node of ('key, 'value) t * 'key * 'value * ('key, 'value) t

  let empty = Empty

  let rec add d k v =
    match d with
    | Empty -> Node (Empty, k, v, Empty)
    | Node (l, k', v', r) ->
      if k = k' then Node (l, k, v, r)
      else if k < k' then Node (add l k v, k', v', r)
      else Node (l, k', v', add r k v)

  exception NotFound

  let rec lookup d k =
    match d with
    | Empty -> raise NotFound
    | Node (l, k', v', r) ->
      if k = k' then v'
      else if k < k' then lookup l k
      else lookup r k

  let rec remove d k =
    let rec append l r =
      match l with
      | Empty -> r
      | Node (ll, kk, vv, rr) -> Node (ll, kk, vv, append rr r)
    in
    match d with
    | Empty -> raise NotFound
    | Node (l, k', v', r) ->
      if k = k' then
        match l, r with
        | Empty, _ -> r
        | _, Empty -> l
        | _, _ -> append l r
      else if k < k' then Node ((remove l k), k', v', r)
      else Node (l, k', v', (remove r k))

end ;;
  
