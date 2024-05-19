type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec size tr = match tr with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size l + size r

let rec depth tr = match tr with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (depth l) (depth r)

let rec insert a tr = match tr with
  | Leaf -> Node(a, Leaf, Leaf)
  | Node (x, l, r) -> if a<x then Node(x, insert a l, Leaf) else Node(x, Leaf, insert a r)

let rec min tr = match tr with
  | Leaf -> failwith "empty"
  | Node (v, Leaf, _) -> v
  | Node (_, l, _) -> min l

let rec max1 tr = match tr with
  | Leaf -> min_int
  | Node(v, l, r) -> max v (max (max1 l) (max1 r))

let rec traverse tr = match tr with
  | Leaf -> []
  | Node(v, l, r) -> traverse l @ [v] @ traverse r

let rec remove e tr = match tr with
  | Leaf -> Leaf
  | Node (v, l, r) -> if e<v then Node (v, remove e l, r) else 
                  if e>v then Node(v, l, remove e r) 
                  else match l, r with
                    | Leaf, Leaf -> Leaf
                    | Leaf, _ -> r
                    | _, Leaf -> l
                    | _ -> Node (min r, l, remove min r)  (*when node has 2 kids*)
