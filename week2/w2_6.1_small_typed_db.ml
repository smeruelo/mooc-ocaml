(* A phone number is a sequence of four integers. *)
type phone_number = int * int * int * int;;

(* A contact has a name and a phone number. *)
type contact = {
    name : string;
    phone_number : phone_number
  };;

(* Here is a dumb contact. *)
let nobody = { name = ""; phone_number = (0, 0, 0, 0) };;

(* A database is a collection of contacts. *)
type database = {
    number_of_contacts : int;
    contacts : contact array;
  };;

(* [make n] is the database with no contact and at most [n] contacts stored inside. *)
let make max_number_of_contacts =
  {
    number_of_contacts = 0;
    contacts = Array.make max_number_of_contacts nobody
  };;

(* Queries are represented by a code and a contact.
   - If the code is 0 then the contact must be inserted.
   - If the code is 1 then the contact must be deleted.
   - If the code is 2 then we are looking for a contact
     with the same name in the database. *)
type query = {
    code : int;
    contact : contact
  };;

(*
let search db contact =
  let rec aux idx =
    if idx >= db.number_of_contacts then
      (false, db, nobody)
    else if db.contacts.(idx).name = contact.name then
      (true, db, db.contacts.(idx))
    else
      aux (idx + 1)
  in
  aux 0;;
*)

let insert db contact =
  if db.number_of_contacts >= Array.length db.contacts then
    (false, db, nobody)
  else
    let (status, db, _) = search db contact in
    if status then (false, db, contact) else
      let cells i =
	    if i = db.number_of_contacts then contact else db.contacts.(i)
      in
      let db' = {
          number_of_contacts = db.number_of_contacts + 1;
          contacts = Array.init (Array.length db.contacts) cells
        }
      in
      (true, db', contact);;

(*
let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
  else
    let cells i =
      if db.contacts.(i).name = contact.name then
        nobody
      else
        db.contacts.(i) in
    let db' = {
        number_of_contacts = db.number_of_contacts - 1;
        contacts = Array.init (Array.length db.contacts) cells
      }
    in
    (true, db', contact);;
*)

(* Engine parses and interprets the query.
let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else (false, db, nobody);;
 *)


let proof_of_bug =
  [| {code = 0; contact = {name = "uno"; phone_number = (1, 1, 1, 1)}};
     {code = 0; contact = {name = "dos"; phone_number = (2, 2, 2, 2)}};
     {code = 1; contact = {name = "uno"; phone_number = (0, 0, 0, 0)}};
     {code = 0; contact = {name = "tres"; phone_number = (3, 3, 3, 3)}};
     {code = 2; contact = {name = "dos"; phone_number = (0, 0, 0, 0)}}|] ;;

let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then
    (false, db, contact)
  else
    let cells i =
      if i = db.number_of_contacts - 1 then
        nobody
      else if db.contacts.(i).name = contact.name then
        db.contacts.(db.number_of_contacts - 1)
      else
        db.contacts.(i) in
    let db' = {
        number_of_contacts = db.number_of_contacts - 1;
        contacts = Array.init (Array.length db.contacts) cells
      } in
    (true, db', contact)

let update db contact =
  let (status, _, _) = engine db {code = 2; contact} in
  if status then
    let cells i =
      if db.contacts.(i).name = contact.name
      then contact
      else db.contacts.(i) in
    let db' = {
        number_of_contacts = db.number_of_contacts;
        contacts = Array.init (Array.length db.contacts) cells
      } in
    (true, db', contact)
  else
    engine db {code = 0; contact}

let engine db { code ; contact } =
  if code = 0 then insert db contact
  else if code = 1 then delete db contact
  else if code = 2 then search db contact
  else if code = 3 then update db contact
  else (false, db, nobody);;
