(* function that reverses the given list *)
let rec customreverse lst = 
  let rec helper lst1 a = match lst1 with
  | [] -> a
  | h::t -> helper t (h::a)
in helper lst []