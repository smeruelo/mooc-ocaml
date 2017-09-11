type phone_number = int * int * int * int

type contact = {
  name : string;
  phone_number : phone_number
}

let nobody = { name = ""; phone_number = (0, 0, 0, 0) }

type database = {
  number_of_contacts : int;
  contacts : contact array;
}

(* [make n] is the database with no contact and at most [n] contacts  stored inside. *)
let make max_number_of_contacts =
  {
    number_of_contacts = 0;
    contacts = Array.make max_number_of_contacts nobody
  }

(* Queries are represented by a code and a contact.
   - If the code is 0 then the contact must be inserted.
   - If the code is 1 then the contact must be deleted.
   - If the code is 2 then we are looking for a contact
     with the same name in the database. *)
type query = {
  code : int;
  contact : contact;
}

let search db contact =
  let rec aux idx =
    if idx >= db.number_of_contacts then
      (false, db, nobody)
    else if db.contacts.(idx).name = contact.name then
      (true, db, db.contacts.(idx))
    else
      aux (idx + 1)
  in
  aux 0

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
      (true, db', contact)

let update db contact =
  let (contact_exists, db, _) = search db contact in
  let make number_of_contacts cells =
    let db' = {
        number_of_contacts = number_of_contacts;
        contacts = Array.init (Array.length db.contacts) cells
      }
    in
    (true, db', contact) in
  if not contact_exists then
    if db.number_of_contacts >= Array.length db.contacts then
      (false, db, nobody)
    else
      let cells i =
        if i = db.number_of_contacts then contact else db.contacts.(i) in
      make (db.number_of_contacts + 1) cells
  else
    let cells i =
      if db.contacts.(i).name = contact.name then contact else db.contacts.(i) in
    make db.number_of_contacts cells

let delete db contact =
  let (status, db, contact) = search db contact in
  if not status then (false, db, contact)
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
      }
    in
    (true, db', contact)

let engine db { code ; contact } =
  if code = 0 then
    insert db contact
  else if code = 1 then
    delete db contact
  else if code = 2 then
    search db contact
  else if code = 3 then
    update db contact
  else
    (false, db, nobody)

let string_of_phone (n1, n2, n3, n4) =
  string_of_int n1 ^ string_of_int n2 ^ string_of_int n3 ^ string_of_int n4

let print_contact { name; phone_number } =
  Printf.printf "name: %s, phone: %s\n" name (string_of_phone phone_number)

let print_db { number_of_contacts; contacts } =
  (*for i = 0 to number_of_contacts - 1 do *)
  for i = 0 to (Array.length contacts - 1) do
    print_contact contacts.(i)
  done


let () =
  let db0 = make 3 in
  print_db db0;
  Printf.printf "number_of_contacts: %d, array.length: %d\n\n" db0.number_of_contacts (Array.length db0.contacts);
  let (_, db1, _) = engine db0 {code = 3; contact = {name = "sonia"; phone_number = (1, 3, 1, 3)}} in
  print_db db1;
  Printf.printf "number_of_contacts: %d, array.length: %d\n\n" db1.number_of_contacts (Array.length db1.contacts);
  let (_, db2, _) = engine db1 {code = 0; contact = {name = "alfredo"; phone_number = (4, 4, 4, 4)}} in
  print_db db2;
  Printf.printf "number_of_contacts: %d, array.length: %d\n\n" db2.number_of_contacts (Array.length db2.contacts);
  let (_, db3, _) = engine db2 {code = 3; contact = {name = "sonia"; phone_number = (8, 8, 8, 8)}} in
  print_db db3;
  Printf.printf "number_of_contacts: %d, array.length: %d\n\n" db3.number_of_contacts (Array.length db3.contacts);
  let (_, _, c) = search db3 {name = "alfredo"; phone_number = (0, 0, 0, 0)} in
  Printf.printf "search alfredo >>> "; print_contact c; Printf.printf "\n"
