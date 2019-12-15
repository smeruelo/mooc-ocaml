type filesystem = (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list;;

let rec print_path = function
  | [] -> ()
  | [x] -> (print_string x; print_newline ())
  | hd :: tl -> print_string (hd ^ "/"); print_path tl;;

let rec print_file lvl name =
  if lvl = 0
  then (print_string name; print_newline ())
  else (print_string "| "; print_file (lvl - 1) name);;

let rec print_symlink lvl name path =
  if lvl = 0
  then (print_string (name ^ " -> "); print_path path)
  else (print_string "| "; print_symlink (lvl - 1) name path);;

let rec print_invalid_symlink lvl name =
  if lvl = 0
  then (print_string (name ^ " -> "); print_string "INVALID"; print_newline ())
  else (print_string "| "; print_invalid_symlink (lvl - 1) name);;

let rec print_dir lvl name =
  if lvl = 0
  then (print_string ("/" ^ name); print_newline ())
  else (print_string "| "; print_dir (lvl - 1) name);;

let print_filesystem root =
  let rec aux lvl = function
    | [] -> ()
    | (name, node) :: tl -> match node with
      | File -> (print_file lvl name; aux lvl tl)
      | Dir fs -> (print_dir lvl name; aux (lvl + 1) fs; aux lvl tl)
      | Symlink path -> (print_symlink lvl name path; aux lvl tl)
  in
  aux 0 root ;;

let rec resolve sym path =
  let rec aux acc path =
    match acc, path with
    | _, [] -> List.rev acc
    | [], ".." :: tl -> aux acc tl
    | _, ".." :: tl -> aux (List.tl acc) tl
    | _, hd :: tl -> aux (hd :: acc) tl
  in
  aux (List.tl (List.rev sym)) path;;

let rec file_exists root path =
  let node_matches ((node1_name : string), node1) ((node2_name : string), node2) =
    match node1, node2 with
    | (File, File) | (Dir _, Dir _) -> node1_name = node2_name
    | (_, _) -> false
  in
  match path with
  | [] -> false
  | [target] ->
    begin
      try
        let _ = List.find (node_matches (target, File)) root in
        true
      with
        Not_found -> false
    end
  | hd :: tl ->
    begin
      try
        match List.find (node_matches (hd, Dir [])) root with
        | target, Dir fs -> file_exists fs (List.tl path)
        | _, _ -> false
      with
        Not_found -> false
    end;;

let print_filesystem root =
  let rec aux abs_path lvl = function
    | [] -> ()
    | (name, node) :: tl -> match node with
      | File -> (print_file lvl name;
                 aux abs_path lvl tl)
      | Dir fs -> (print_dir lvl name;
                   aux (name :: abs_path) (lvl + 1) fs;
                   aux abs_path lvl tl)
      | Symlink path ->
        if file_exists root (resolve (List.rev (name :: abs_path)) path) then
          (print_symlink lvl name path;
           aux abs_path lvl tl)
        else
          (print_invalid_symlink lvl name;
           aux abs_path lvl tl)
  in aux [] 0 root ;;
