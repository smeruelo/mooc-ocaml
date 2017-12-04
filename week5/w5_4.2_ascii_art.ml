type image = int -> int -> bool

let all_white = fun x y -> false

let all_black = fun x y -> true

let checkers = fun x y -> y/2 mod 2 = x/2 mod 2

let square cx cy s = fun x y ->
  let minx = cx - s / 2 in
  let maxx = cx + s / 2 in
  let miny = cy - s / 2 in
  let maxy = cy + s / 2 in
  x >= minx && x <= maxx && y >= miny && y <= maxy

let disk cx cy r = fun x y ->
  let x' = x - cx in
  let y' = y - cy in
  (x' * x' + y' * y') <= r * r

type blend =
  | Image of image
  | And of blend * blend
  | Or of blend * blend
  | Rem of blend * blend


let display_image width height f_image =
  for y = 0 to height + 1 do
    for x = 0 to width + 1 do
      if f_image x y
      then print_char '#'
      else print_char ' '
    done;
    print_newline ()
  done


let rec render blend x y =
  match blend with
  | Image b -> b x y
  | And (b1, b2) -> not ((render b1) x y) && not ((render b2) x y)
  | Or (b1, b2) -> not ((render b1) x y) || not ((render b2) x y)
  | Rem (b1, b2) -> not ((render b1) x y) && ((render b2) x y)


let display_blend width height blend =
  display_image width height (render blend)
