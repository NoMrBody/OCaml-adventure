(*  Extract a Slice From a List  *)
(* # slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 5;;
   - : string list = ["c"; "d"; "e"; "f"] *)
let rec slice lst st fin = 
  let rec drop lst st = match lst with
    | [] -> []
    | h::t as xxs-> if st>0 then drop t (st-1) else xxs
  in let rec helper list fin acc = match list with 
    | [] -> List.rev acc
    | h::t -> if fin<0 then List.rev acc else helper t (fin-1) (h::acc)
in helper (drop lst st) (fin-st) [] 