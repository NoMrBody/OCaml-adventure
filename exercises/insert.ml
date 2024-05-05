(* function that inserts an element at the given index *)
let rec insert lst elmnt index = if index < 0 then failwith "Negative index" else
   match lst with
  | [] -> [elmnt]
  | h::t -> if index=0 then elmnt::lst else h::insert t elmnt (index-1)