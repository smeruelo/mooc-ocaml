type point  = { x : float; y : float; z : float }
type dpoint = { dx : float; dy : float; dz : float }
type physical_object = { position : point; velocity : dpoint }

let move p dp =
  { x = p.x +. dp.dx; y = p.y +. dp.dy; z = p.z +. dp.dz }

let next obj =
  { position = move obj.position obj.velocity ; velocity = obj.velocity}

let will_collide_soon obj1 obj2 =
  let nobj1 = next obj1
  and nobj2 = next obj2 in
  let distance p1 p2 =
    sqrt ((p2.x -. p1.x) ** 2. +. (p2.y -. p1.y) ** 2. +. (p2.z -. p1.z) ** 2.)
  in distance nobj1.position nobj2.position < 2.

(*
will_collide_soon {position = {x = 1.; y = 2.; z = 1.}; velocity = {dx = 0.; dy = 0.; dz = 1.}}
                  {position = {x = 1.; y = 2.; z = 1.8}; velocity = {dx = 0.; dy = 0.; dz = 0.1}};;

will_collide_soon {position = {x = 1.; y = 2.; z = 1.}; velocity = {dx = 0.; dy = 0.; dz = 1.}}
                  {position = {x = 1.; y = 2.; z = 4.8}; velocity = {dx = 0.; dy = 0.; dz = 0.1}};;
*)
