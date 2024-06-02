(* Drop Every N'th Element From a List  *)
let rec drop_every_nth lst n = 
  let rec helper lst n acc i = match lst with
    | [] -> acc
    | h::t -> if n <> i then helper t n (h::acc) (i+1) else helper t n acc 1
in List.rev @@ helper lst n [] 1
