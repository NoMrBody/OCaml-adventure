(* function that appends lst1 to lst2 *)
let rec customappend lst1 lst2 = match lst1 with
| [] -> lst2
| h::t -> h::customappend t lst2