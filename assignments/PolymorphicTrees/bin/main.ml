type 'a tree = Empty | Node of 'a * 'a tree * 'a tree 

let rec insert compare el tr = match tr with
  | Empty -> Node(el, Empty, Empty)
  | Node(v,l,r) as t -> let num = compare el v in if (num=0) then t 
  else if num<0 then Node(v, insert compare el l, r) 
  else Node(v, l,insert compare el r)

