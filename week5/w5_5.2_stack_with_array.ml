type stack = int array
exception Full
exception Empty

let create size =
  let a = Array.init (size + 1) (fun _ -> 0) in a

let top buf =
  buf.(0)

let is_full buf =
  top buf = (Array.length buf) - 1

let is_empty buf =
  top buf = 0

let print_stack buf =
  for i = top buf downto 1 do
    print_int i; print_newline ()
  done

let push buf elt =
  if is_full buf then
    raise Full
  else
    begin
      buf.(0) <- (top buf) + 1;
      buf.(top buf) <- elt
    end

let append buf arr =
  for i = Array.length(arr) - 1 downto 0 do
    push buf arr.(i)
  done

let pop buf =
  if is_empty buf then
    raise Empty
  else
    begin
      buf.(0) <- (top buf) - 1;
      buf.((top buf) + 1)
    end
