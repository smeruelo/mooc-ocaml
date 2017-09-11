let exchange k =
  if k > 9 && k < 100 then
    let ones = k mod 10
    and tens = k / 10
    in ones * 10 + tens
  else (* raise exception *)
    k

let is_valid_answer (grand_father_age, grand_son_age) =
  grand_son_age * 4 == grand_father_age &&
  (exchange grand_father_age) * 3 == exchange grand_son_age

let find (max_gf, min_gs) =
  let rec aux gs =
    if gs <= max_gf / 4 then
      let gf = gs * 4 in
      if is_valid_answer (gf, gs) then
        (gf, gs)
      else
        aux (gs + 1)
    else
      (-1, -1)
  in aux min_gs
