(* Interleave 2 *)
(* output should look like this [1;2;3], [0;0] -> [1;0;2;0;3] *)
let rec interleave2 lst1 lst2 = match lst1, lst2 with
  | [], [] -> []
  | h::t, [] | [], h::t -> h::t
  | h1::t1, h2::t2 -> h1::h2::interleave2 t1 t2

(* Interleave 3 *)
let rec interleave3 lst1 lst2 lst3 = match lst1, lst2, lst3 with
  | [], [], [] -> []
  | h::t, [], [] | [], h::t, [] | [], [], h::t-> h::t
  | h1::t1, h2::t2, [] | h1::t1, [], h2::t2 | [], h1::t1, h2::t2 -> h1::h2::interleave2 t1 t2
  | h1::t1, h2::t2, h3::t3 -> h1::h2::h3::interleave3 t1 t2 t3
