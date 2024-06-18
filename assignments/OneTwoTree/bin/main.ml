type 'a onetwo = Null | One of 'a * 'a onetwo | Two of 'a onetwo * 'a * 'a onetwo

(* extracts the least element out of a tree and return this value (whenever possible)
as an optional value togerther with the remaining elements *)
let rec extract_min tr = match tr with
  | Null -> None, Null
  | One(a, t) -> Some a, t
  | Two(l, a, r) -> match extract_min l with
    | None, _t -> Some a, r
    | Some k, t' -> Some k, Two(t', a, r)


(* just an in-order traversal on a binary tree *)
let rec inorder tr = match tr with
  | Null -> []
  | One(a, t) -> a::inorder t
  | Two(l, a, r) -> inorder l @ (a::inorder r)


(* checks whether the elements in a tree form a strictly increasing sequence *)
  let verify tr = 
  let traversed = inorder tr in
  let sorted = List.sort compare traversed in
  sorted = traversed


(* transform a tree so that the resulting tree has the same sequence of elements
but no One nodes anymore *)
let rec normal tr = match tr with
  | Null -> Null
  | One(a, t) -> Two(Null, a, normal t)
  | Two(l, a, r) -> Two(normal l, a, normal r)

  
(* constructs onetwo tree out of an unordered list *)
let rec from_list lst = 
  let refined = List.sort_uniq compare lst in 
  match refined with
  | [] -> Null
  | h::t -> One(h, from_list t)
