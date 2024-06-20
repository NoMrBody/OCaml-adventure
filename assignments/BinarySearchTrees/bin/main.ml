type tree = Leaf | Node of int * tree * tree

let rec node v l r = Node(v, l, r)
let leaf _ = Leaf

let inspect tr = match tr with
  | Leaf -> None
  | Node(v, l, r) -> Some (v, l, r)

(* returns an ordered list of all values in tree *)
let rec to_list tr = match tr with
  | Leaf -> []
  | Node(v, l, r) -> to_list l @ (v::to_list r)

(* inserts a value into the tree, if one doesn't exist - tree is not modified *)
let rec insert e tr = match tr with
  | Leaf -> Node(e, Leaf, Leaf)
  | Node(v, l, r) -> if e=v then Node(v, l, r) else match e<v with
    | true -> insert e l
    | false -> insert e r

(* aux *)
let rec remove_max tr = match tr with
  | Leaf -> failwith "bwoah"
  | Node(v, l, Leaf) -> v, l
  | Node(v, l, r) -> match remove_max r with
    | v', r' -> v', Node(v, l, r')


let rec remove el tr = match tr with
  | Leaf -> Leaf
  | Node(v, l, r) -> 
    if el<v then remove el l 
    else if el>v then remove el r 
    else match l, r with
      | Leaf, _ -> r
      | _, Leaf -> l
      | _, _ -> let v', l' = remove_max l in Node(v', l', r)