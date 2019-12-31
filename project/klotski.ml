(* --- Given prelude --- *)

exception NotFound ;;

type 'e rel = 'e -> 'e list ;;
type 'e prop = 'e -> bool ;;

type ('a, 'set) set_operations = {
  empty : 'set;              (* The empty set. *)
  mem : 'a -> 'set -> bool;  (* [mem x s = true] iff [x] is in [s]. *)
  add : 'a -> 'set -> 'set;  (* [add s x] is the set [s] union {x}. *)
} ;;

type ('configuration, 'move) puzzle = {
  move : 'configuration -> 'move -> 'configuration;
  possible_moves : 'configuration -> 'move list;
  final : 'configuration -> bool
} ;;

type piece_kind = S | H | V | C | X ;;
type piece = piece_kind * int ;;
let x = (X, 0) and s = (S, 0) and h = (H, 0) ;;
let (c0, c1, c2, c3) = ((C, 0), (C, 1), (C, 2), (C, 3)) ;;
let (v0, v1, v2, v3) = ((V, 0), (V, 1), (V, 2), (V, 3)) ;;
let all_pieces : piece list = [ s; h; c0; c1; c2; c3; v0; v1; v2; v3 ] ;;

type board = piece array array ;;
let initial_board =
  [| [| v0 ; s  ; s  ; v1 |];
     [| v0 ; s  ; s  ; v1 |];
     [| v2 ; h  ; h  ; v3 |];
     [| v2 ; c0 ; c1 ; v3 |];
     [| c2 ; x  ; x  ; c3 |] |] ;;

let initial_board_simpler =
  [| [| c2 ; s  ; s  ; c1 |] ;
     [| c0 ; s  ; s  ; c3 |] ;
     [| v1 ; v2 ; v3 ; v0 |] ;
     [| v1 ; v2 ; v3 ; v0 |] ;
     [| x  ; x  ; x  ; x  |] |] ;;

let initial_board_trivial =
  [| [| x  ; s  ; s  ; x  |] ;
     [| x  ; s  ; s  ; x  |] ;
     [| x  ; x  ; x  ; x  |] ;
     [| x  ; x  ; x  ; x  |] ;
     [| x  ; x  ; x  ; x  |] |] ;;

type direction = { dcol : int; drow : int; } ;;
type move = Move of piece * direction * board ;;
let move _ (Move (_, _, b)) = b ;;


(* --- Preliminaries --- *)

let rec loop p f x =
  if p x
  then x
  else loop p f (f x) ;;

let rec find p = function
  | [] -> raise NotFound
  | hd :: tl when p hd = true -> hd
  | _ :: tl -> find p tl ;;

let rec exists p l =
  try
    ignore (find p l); true
  with NotFound -> false ;;


(* --- Part A: A Generic Problem Solver --- *)

let near : int rel =
  fun x -> [x - 2; x - 1; x; x + 1; x + 2] ;;

let flat_map (r : 'e rel) =
  fun l -> List.concat (List.map r l) ;;

let rec iter_rel (r : 'e rel) (n : int) =
  fun x -> match n with
    | 0 -> [x]
    | i -> flat_map r (iter_rel r (n - 1) x) ;;

let solve (r : 'a rel) (p : 'a prop) (x : 'a) =
  find p (loop (exists p) (flat_map r) [x]) ;;

let solve_path (r : 'a rel) (p : 'a prop) (x : 'a) =
  let p' = fun l -> p (List.hd l)
  and r'= fun l -> List.map (fun e -> e :: l) (r (List.hd l))
  in List.tl (List.rev (solve r' p' [x; x])) ;;

let archive_map (opset : ('a, 'set) set_operations) (r : 'a rel) (s, l) =
  let rec aux s' l' = function
    | [] -> (s', l')
    | hd :: tl ->
        if not (opset.mem hd s')
        then aux (opset.add hd s') (hd :: l') tl
        else aux s' l' tl
  in aux s [] (flat_map r l) ;;

let solve' (opset : ('a, 'set) set_operations) (r : 'a rel) (p : 'a prop) (x : 'a) =
  let exists' (s, l) = exists p l
  and find' (s, l) = find p l
  in find' (loop exists' (archive_map opset r) (opset.empty, [x])) ;;

let solve_path' (opset : ('a list, 'set) set_operations) (r : 'a rel) (p : 'a prop) (x : 'a) =
  let p' = fun l -> p (List.hd l)
  and r' = fun l -> List.map (fun e -> e :: l) (r (List.hd l))
  in List.tl (List.rev (solve' opset r' p' [x; x])) ;;

let solve_puzzle (p : ('c, 'm) puzzle) (opset : ('c list, 'set) set_operations) (c : 'c) =
  let r c = List.map (p.move c) (p.possible_moves c)
  and pred c = p.final c
  in solve_path' opset r pred c ;;


(* --- Part B: A Solver for Klotski --- *)

let final board =
  board.(3).(1) = s && board.(3).(2) = s && board.(4).(1) = s && board.(4).(2) = s ;;

type location = { row : int; col : int} ;;

(* List of locations occupied by a piece *)
let piece_cells (b : board) (p : piece) =
  let rec aux accum = function
    | 20 -> accum
    | i -> let r = (i / 4) and c = (i mod 4) in
      if b.(r).(c) = p
      then aux ({ row = r ; col = c } :: accum) (i + 1)
      else aux accum (i + 1)
  in aux [] 0 ;;

let copy_board (b : board) =
  let b' = [| [||] ; [||] ; [||] ; [||] ; [||] |] in
  for row = 0 to 4 do
    b'.(row) <- Array.copy b.(row)
  done;
  b' ;;

let move_piece (b : board) (p : piece) { drow; dcol } =
  let in_bounds loc =
    loc.row >= 0 && loc.row < 5 && loc.col >= 0 && loc.col < 4
  and free loc =
    b.(loc.row).(loc.col) = x || b.(loc.row).(loc.col) = p in
  let cells = piece_cells b p in
  let new_cells =
    List.map (fun loc -> { row = loc.row + drow ; col = loc.col + dcol }) cells in
  if List.for_all in_bounds new_cells && List.for_all free new_cells then
    let b' = copy_board b in
    List.iter (fun loc -> b'.(loc.row).(loc.col) <- x) cells;
    List.iter (fun loc -> b'.(loc.row).(loc.col) <- p) new_cells;
    Some b'
  else
    None ;;

let possible_moves (b : board) =
  let all_dir = [ { drow = 0; dcol = -1 }
                ; { drow = 0; dcol = 1 }
                ; { drow = -1; dcol = 0 }
                ; { drow = 1; dcol = 0 } ]
  and define_move = fun p dir ->
    match move_piece b p dir with
    | None -> None
    | Some b' -> Some (Move (p, dir, b')) in
  let rec filter_moves accum = function
    | [] -> accum
    | hd :: tl -> match hd with
      | None -> filter_moves accum tl
      | Some m -> filter_moves (m :: accum) tl in
  filter_moves [] (List.concat (List.map (fun p -> List.map (define_move p) all_dir) all_pieces)) ;;

let klotski : (board, move) puzzle = { move; possible_moves; final } ;;

module BoardSet = Set.Make (struct
    type t = board
    let compare b1 b2 =
      let rec aux r row1 row2 c =
        let p1 = row1.(c)
        and p2 = row2.(c) in
        match p1, p2 with
        | (type1, num1), (type2, num2) when type1 = type2 && num1 = num2 ->
          if c = 3 then
            if r = 4
            then 0
            else aux (r + 1) b1.(r + 1) b2.(r + 1) 0
          else
            aux r row1 row2 (c + 1)
        | (type1, num1), (type2, num2) when type1 = type2 && num1 != num2 -> compare num1 num2
        | (X, _), (_, _) -> -1
        | (_, _), (X, _) -> 1
        | (V, _), (_, _) -> -1
        | (_, _), (V, _) -> 1
        | (C, _), (_, _) -> -1
        | (_, _), (C, _) -> 1
        | (H, _), (_, _) -> -1
        | (_, _), (H, _) -> 1
        | _, _ -> 1
      in aux 0 b1.(0) b2.(0) 0 ;;
  end) ;;

let solve_klotski initial_board =
  "Replace this string with your implementation." ;;
