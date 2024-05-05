(* function that returns the greatest element of the list *)
let rec max lst = match lst with
| [] -> None
| h::t -> if h > max(t) then h else max(t)