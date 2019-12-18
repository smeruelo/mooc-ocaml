type 'a xlist =
  { mutable pointer : 'a cell }
and 'a cell =
  | Nil
  | List of 'a * 'a xlist ;;

let nil () =
  { pointer = Nil } ;;

let cons elt rest =
  { pointer = List (elt, rest) } ;;

exception Empty_xlist ;;

let head l =
  match l.pointer with
  | Nil -> raise Empty_xlist
  | List (hd, tl) -> hd

let tail l =
  match l.pointer with
  | Nil -> raise Empty_xlist
  | List (hd, tl) -> tl

let add a l =
  l.pointer <- List (a, { pointer = l.pointer })

(* Using the provided functions *)
let add a l =
  match l.pointer with
  | Nil -> l.pointer <- List (a, nil ())
  | List (hd, tl) -> l.pointer <- List (a, cons hd tl)

let chop l =
  match l.pointer with
  | Nil -> raise Empty_xlist
  | List (hd, tl) -> l.pointer <- tl.pointer

let rec append l1 l2 =
  match l1.pointer with
  | Nil -> l1.pointer <- l2.pointer
  | List (hd, tl) -> append tl l2

let rec filter p l =
  match l.pointer with
  | Nil -> ()
  | List (hd, tl) when p hd -> filter p tl
  | List (hd, tl) -> l.pointer <- tl.pointer; filter p l

(*
let xl = { pointer = List (1, { pointer = List (2, { pointer = List (3, { pointer = Nil }) }) }) }
let even a = a mod 2 == 0
*)
