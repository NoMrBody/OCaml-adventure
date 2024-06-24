type 'a tree = 
  | Leaf of 'a
  | One of int * 'a tree
  | Two of int * int * 'a tree * 'a tree

(* val search : 'a tree -> int -> 'a option *)
let rec search t i = match t with
  | Leaf v -> if i=0 then Some v else None
  | One (_n, t') -> search t' i
  | Two (_, _, l, r) -> 
    let remaining_left = match l with
      | Leaf _ -> 1
      | One (n',_) -> n'
      | Two (n', _, _, _) -> n' in 
    if i < remaining_left then search l i 
    else search r (i-remaining_left)

(* val update : 'a tree -> int -> 'a -> bool * 'a tree *)
let rec update tree index item = match tree with
   | Leaf v -> if index=0 then (true, Leaf item) else (false, Leaf v)
   | One(n, t) -> let (status, t') = update t index item in 
     (status, One(n, t'))
   | Two(n,m,l,r) -> 
     let remaining_left = match l with
      | Leaf _ -> 1
      | One (n', _) -> n'
      | Two (n',_,_,_) -> n' in
    if index < remaining_left then let (status, t') = update l index item in
      (status, Two(n, m, t', r)) 
   else let (status, t') = update r (index-remaining_left) item in
      (status, Two(n, m, l, t'))
     
(* val insert : 'a tree -> int -> 'a -> 'a tree *)
let rec insert tree index item = match tree with
  | Leaf v -> if index=0 then Two(1, 1, Leaf item, Leaf v)
    else if index=1 then Two(1, 1, Leaf v, Leaf item)
    else failwith "bwoah, invalid index"
  | One(n, t') -> if index<n then insert t' index item
    else One(n+1, Two(n, 1, t', Leaf item))
  | Two (n1, n2, l, r) -> if (index > n1+n2) then failwith "index out of bounds"
    else if index < n1 then Two(n1+1,n2, insert l index item, r)
    else Two(n1, n2+1, l, insert r (index-n1) item)