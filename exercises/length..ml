(* function that returns the length of a list *)
let rec len1 lst = 
  let rec help lst acc = match lst with
  | [] -> acc
  | h::t -> help t (1+acc)
in help lst 0 