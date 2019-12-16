let rotate a =
  let n = Array.length a in
  match n with
  | 0 -> ()
  | _ ->
    let v = a.(0) in
    for i = 0 to n-2 do
      a.(i) <- a.(i+1)
    done;
    a.(n-1)<-v ;;

let rotate_by a k =
  let n = Array.length a in
  match n with
  | 0 -> ()
  | n ->
    for i = 1 to (n + k) mod n do
      rotate a
    done
