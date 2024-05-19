let rec fold_left f a l = match l with
| [] -> a
| h::t -> fold_left f (f a h) t;; 

(* f1 >> length*)
fold_left (fun a _ -> a+1) (0) [1;2;3];;

(* f2 >> returns longest list*)
fold_left (fun a x -> if List.length a > List.length x then a else x) [] [[1;2]; [4;5;6]; []; [3]];;

(* f3 >> returns reversed tuples*)
fold_left (fun a (x,y) -> a @ [y,x]) [] [(1,2); (3,4)];;

(* f4 [1;2;3;4;5;6] -> [5;3;1;2;4;6] *)
fold_left (fun a x -> if List.length a mod 2 = 0 then x::a else a @ [x]) [] [1;2;3;4;5;6];;

(* f5 *)
let g = fold_left (fun f (a, b) -> fun x -> if x = a then b else f x)  (fun _ -> failwith "bwoah") [1,2; 3,4; 5,7];;

(* f6 *)
(* let f1 x = x;;
let f2 x = x+1;;
let f3 x = x+2;;
fold_left (fun (h::t) f -> f h::(h::t)) [0] [f1; f2; f3];; *)
