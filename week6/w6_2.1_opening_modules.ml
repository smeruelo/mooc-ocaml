open Digest
open String
open List

let print_hashes (hashes : Digest.t list) : unit =
  let print_hash h = h |> to_hex |> uppercase_ascii |> print_endline in
  iter print_hash hashes;;
