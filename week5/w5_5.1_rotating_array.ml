let rotate a =
  print_char '*';
  let n = Array.length a in
  if n > 1 then
    let v = a.(0) in
    for i = 0 to n-2 do
      a.(i) <- a.(i+1)
    done;
    a.(n-1)<-v


let rotate_by a n =
  let l = Array.length a in
  let real_n = n mod l in
  if real_n != 0 then
    for i = 1 to real_n do
      rotate a
    done
