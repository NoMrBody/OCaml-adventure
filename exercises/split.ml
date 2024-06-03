(* Split a List Into Two Parts; The Length of the First Part Is Given *)
let rec split lst len = 
  let rec helper lst len acc1 acc2 = match lst with
    | [] -> List.rev acc1, acc2
    | h::t -> if len > 0  then helper t (len-1) (h::acc1) t else List.rev acc1, acc2
in helper lst len [] []