let rec custom_insert lst i e = match lst with
  | [] -> if i=0 then [e] else failwith "invalid index"
  | h::t -> if i=0 then e::lst else h::custom_insert t (i-1) e