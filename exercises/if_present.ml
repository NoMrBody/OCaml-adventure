(* function that returns whether inputted element is in the list or not *)
let rec find element list =
  match list with
  |  [] -> false
  |  h::t -> if h = element then true else find element t