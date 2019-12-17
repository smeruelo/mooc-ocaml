type stack = int array;;
exception Full;;
exception Empty;;

let create size =
  let a = Array.make (size + 1) 0 in
  a;;

let push buf elt =
  if buf.(0) = ((Array.length buf) - 1) then
    raise Full
  else
    begin
      buf.(buf.(0) + 1) <- elt;
      buf.(0) <- buf.(0) + 1
    end;;

let append buf arr =
  for i = (Array.length arr) - 1 downto 0 do
    push buf arr.(i)
  done;;

let pop buf =
  if buf.(0) = 0 then
    raise Empty
  else
    let top = buf.(buf.(0)) in
    buf.(0) <- buf.(0) - 1;
    top;;
