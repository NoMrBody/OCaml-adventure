(* function that filters out even elements of the integer list *)
let rec filter_evens lst = 
  let rec test n = if (n mod 2 = 0) then true else false
in match lst with 
| [] -> []
| h::t -> if test h then h::filter_evens t else filter_evens t


(* function that returns even numbered elements of the list *)
let rec evens lst = match lst with 
| [] -> []
| _::[] -> []
| _::m::t -> m::evens t