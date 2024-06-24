type 'a tree = 
  | Leaf of 'a
  | One of int * 'a tree
  | Two of int * int * 'a tree * 'a tree


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
