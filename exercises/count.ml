(* function that counts true elements of the list *)
let rec truzz lst acc = match lst with
| [] -> acc
| h::t -> if (h=true) then truzz t (acc+1) else truzz t acc

