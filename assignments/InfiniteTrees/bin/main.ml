type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec layer_tree r = LNode (r, (fun () -> layer_tree(r+1)), (fun () -> layer_tree(r+1)))

let rec interval_tree l h =
  LNode ((l, h), (fun () -> interval_tree l ((l+.h)/.2.)), (fun () -> interval_tree l ((l+.h)/.2.)))

let rec rational_tree n d = 
  LNode ((n, d), (fun () -> rational_tree n (d+1)), (fun () -> rational_tree n (d+1)))

let rec top n t = let LNode(v, l, r)=t in 
  if n=0 then Leaf else Node(v, top (n-1) (l ()), top (n-1) (r ()))

let rec map f t = let LNode(v, l, r)=t in
  LNode(f v, (fun () -> map f (l ())), (fun () -> map f (r ())))

let rec find pred t = 
  let rec traverse lst = match lst with
    | [] -> None
    | h::t -> let LNode(v, l, r)=h in if pred v then Some h
      else traverse (t@[l ()]@[r ()])
  in traverse [t]
