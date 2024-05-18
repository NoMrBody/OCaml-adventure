(* function that packs consecutive duplicates of list elements into sublists *)
let rec pack lst = 
  let rec helper curr acc lst = match lst with
  | [] -> []
  | [a] -> (a::curr)::acc 
  | h::(m::_ as t) -> 
      if h=m then helper (h::curr) acc t
      else helper [] ((h::curr)::acc) t
  in List.rev @@ helper [] [] lst