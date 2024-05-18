let rec rle lst = 
  let rec helper acc cnt lst = match lst with
  | [] -> []
  | [a] -> ((cnt+1), a)::acc 
  | h::(m::_ as t) -> 
      if h=m then helper acc (cnt+1) t 
      else helper  (((cnt+1), h)::acc) 0 t
  in List.rev @@ helper [] 0 lst