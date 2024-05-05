(* function that rises x to the nth power *)
let rec power x n = match n with
| 0 -> 1
| n when n<0 -> 1 / power x (n-1)
| _ -> x * power x (n-1) 
