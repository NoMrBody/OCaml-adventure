(* find the sum of list elements *)
let sum_list lst = 
  List.fold_left (fun acc x -> acc + x) 0 lst


(* compute the product of all numbers in list *)
let prod_list lst = 
  List.fold_left (fun acc x -> acc * x) 1 lst


(* find the max element in the list *)
let max_list lst = 
  List.fold_left (fun acc x -> if x > acc then x else acc) min_int lst


  (* reverse the list *)
let reverse_list lst = 
  List.fold_left (fun acc x -> x :: acc) [] lst


  (* find if element is in the list *)
let find lst el = 
  List.fold_left (fun acc x -> x=el || acc) false lst 


  (* concatenate all strings in list *)
let concat lst = 
  List.fold_left (fun acc x -> acc^x) "" lst


  (* convert all list elements(integer) into string *)
let convert_to_st lst = 
  List.fold_left (fun acc x -> acc ^ string_of_int x ) "" lst


  (* filter list *)
let filter predicate lst = 
  List.fold_left (fun acc x -> if predicate x then acc @ [x] else acc) [] lst
  

  (* function that converts list into set *)
let to_set lst = 
  let rec present lst el = match lst with
  | [] -> false
  | h::t -> h = el || present t el
in List.rev @@ List.fold_left (fun acc x -> if present acc x then acc else x::acc) [] lst


(* function that eliminates duplicate elements from a list *)
let rec compress lst = 
  let rec test el lst = match lst with
  | [] -> false
  | h::t -> h=el || test el t
in List.rev @@ List.fold_left (fun acc x -> if test x acc then acc else x::acc) [] lst

