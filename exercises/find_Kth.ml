(* function that finds K-th element of the list *)
let rec find_Kth lst index = 
  let rec head lst = match lst with 
    | [] -> []
    | h::_ -> [h]
  in let rec tail lst = match lst with 
    | [] -> []
    | _::t -> t
  in match index with
  | 0 -> head lst
  | _ -> find_Kth (tail lst) (index-1)