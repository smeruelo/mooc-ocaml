type filesystem =
  (string * node) list
and node =
  | File
  | Dir of filesystem
  | Symlink of string list


let rec print_path path =
  match path with
  | [] -> Printf.printf ""
  | hd :: [] -> Printf.printf "%s" hd
  | hd :: tl -> Printf.printf "%s" (hd ^ "/"); print_path tl

let rec print_file lvl name =
  match lvl with
  | 0 -> Printf.printf "%s" (" " ^ name)
  | i -> Printf.printf "%s" (" |"); print_file (i - 1) name

let rec print_symlink lvl name path =
  match lvl with
  | 0 -> Printf.printf "%s" (" " ^ name ^ " -> "); print_path path
  | i -> Printf.printf "%s" " |"; print_symlink (lvl - 1) name path

let rec print_invalid lvl name path =
  match lvl with
  | 0 -> Printf.printf "%s" (" " ^ name ^ " -> INVALID")
  | i -> Printf.printf "%s" " |"; print_invalid (lvl - 1) name path

let rec print_dir lvl name =
  match lvl with
  | 0 -> Printf.printf "%s" (" /" ^ name)
  | i -> Printf.printf "%s" " |"; print_dir (lvl -1) name

let print_filesystem root =
  let rec print_filesystem lvl items =
    match items with
    | [] -> ()
    | hd :: tl ->
      begin
        match hd with
        | (name, File) -> print_file lvl name; print_newline ()
        | (name, Dir fs) ->
          print_dir lvl name; print_newline ();
          print_filesystem (lvl + 1) fs
        | (name, Symlink path) -> print_symlink lvl name path; print_newline ()
      end;
      print_filesystem lvl tl
  in print_filesystem 0 root

let resolve sym path =
  let rec aux abs rel =
    match rel with
    | [] -> List.rev abs
    | ".." :: tl_rel -> aux (List.tl abs) tl_rel
    | hd_rel :: tl_rel -> aux (hd_rel :: abs) tl_rel
  in aux (List.tl (List.rev sym)) path

let same_type_and_name ((name1 : string), node1) ((name2 : string), node2) =
  match node1, node2 with
  | (File, File) -> name1 = name2
  | (Dir fs1, Dir fs2) -> name1 = name2
  | (Symlink path1, Symlink path2) -> name1 = name2
  | (_, _) -> false

let rec file_exists root path =
  match path with
  | last :: [] ->
    begin
      try
        let (_, _) = List.find (same_type_and_name (last, File)) root in
        true
      with
        Not_found -> false
    end
  | current :: tl ->
    begin
      try
        let (name, Dir dir) = List.find (same_type_and_name (current, Dir [])) root in
        file_exists dir tl
      with
        Not_found -> false
    end

let print_filesystem root =
  let rec print_filesystem current_path items =
    match items with
    | [] -> ()
    | hd :: tl ->
      begin
        let lvl = List.length current_path in
        match hd with
        | (name, File) ->
          print_file lvl name; print_newline ()
        | (name, Dir fs) ->
          print_dir lvl name; print_newline ();
          print_filesystem (current_path @ [name]) fs
        | (name, Symlink path) ->
          if file_exists root (resolve (current_path @ [name]) path)
          then begin print_symlink lvl name path; print_newline () end
          else begin print_invalid lvl name path; print_newline () end
      end;
      print_filesystem current_path tl
  in print_filesystem [] root
