let f1 acc (a, b) = acc @ [b, a]

let f2 acc x = if (List.length acc) mod 2 = 0 then x::acc else List.rev(x::List.rev acc)

let f3 acc (k, v) = (fun x -> if x=k then v else acc x)

(* map function in a tail recurseive form *)
let rec map_tr f lst = 
  let rec helper f lst acc = match lst with
    | [] -> List.rev acc
    | h::t -> helper f t (f h :: acc)
  in helper f lst []

(* replication function in a tail recursive form *)
  let rec replicate_tr n x = 
  let rec helper n x acc = if n<1 then acc else helper (n-1) x (x::acc) 
  in helper n x []

(* definition of lazy lists *)
type 'a custom_llist = (unit -> 'a custom_cell) and 'a custom_cell = NillC | ConsC of ('a * 'a custom_llist)
type 'a ocaml_llist = 'a ocaml_cell Lazy.t and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)

(* mapping *)
let rec map_over_custom_llist f lst = 
  fun () -> match lst() with
  | NillC -> NillC
  | ConsC (h, t) -> ConsC (f h, map_over_custom_llist f t)


let rec map_over_ocaml_llist f lst = 
  lazy(
    match Lazy.force lst with
    | NilO -> NilO
    | ConsO (h, t) -> ConsO (f h, map_over_ocaml_llist f t)
  )

(* merging *)
let rec merge_custom_llists lst1 lst2 = 
  fun () -> match lst1(), lst2() with
    | NillC, _ -> lst2()
    | _, NillC -> lst1()
    | ConsC (h1, t1), ConsC (h2, t2) -> 
      if h1<=h2 
        then ConsC (h1, merge_custom_llists t1 lst2)
      else ConsC (h2, merge_custom_llists lst1 t2)


let rec merge_ocaml_llists lst1 lst2 = 
  lazy(
    match Lazy.force lst1, Lazy.force lst2 with
    | NilO, _ -> Lazy.force lst2
    | _, NilO -> Lazy.force lst1
    | ConsO (h1, t1), ConsO (h2, _t2) -> 
      if h1<=h2 then 
        ConsO (h1, merge_ocaml_llists t1 lst2)
      else ConsO (h2, merge_ocaml_llists lst1 t1) 
  )

(* dropping duplicates *)
let rec drop_dupl_custom_llist l = 
  fun () -> 
    match l() with
    | NillC -> NillC
    | ConsC (h, t) -> 
      match t() with
      | NillC -> ConsC(h, fun () -> NillC)
      | ConsC (h1, _t1) -> 
        if h=h1 then drop_dupl_custom_llist t ()
        else ConsC (h, drop_dupl_custom_llist t)


let rec drop_dupl_ocaml_llist l =
  lazy(
    match Lazy.force l with
    | NilO -> NilO
    | ConsO (h, t) -> 
      match Lazy.force t with
      | NilO -> ConsO (h, lazy NilO)
      | ConsO (h1, _t1) -> 
          if h=h1 then Lazy.force (drop_dupl_ocaml_llist t)
          else ConsO (h, drop_dupl_ocaml_llist t)
  )


(*  *)

let rec div_till_prime n p = if (n mod p) = 0 then div_till_prime (n/p) p else n

(* Hamming numbers are numbers that have only 2, 3, and 5 as their prime divisors. 
   For example, 15 is a Hamming number because its only prime divisors are 3 and 5, 
   whereas 21 is not a Hamming number because it has 7 as a prime divisor. *)
let rec check_hamming = function 
 | 1 -> true
 | x -> 
      if x mod 2 = 0 then check_hamming (div_till_prime x 2)
      else if x mod 3 = 0 then check_hamming (div_till_prime x 3)
      else if x mod 5 = 0 then check_hamming (div_till_prime x 5)
      else false

let rec hamming_custom () = 
  let rec helper n = 
    if check_hamming n then ConsC(n, fun () -> helper(n+1)) else helper (n+1)
  in helper 1


  let rec hamming_ocaml = 
    let rec helper n = 
      if check_hamming n then ConsO(n, lazy (helper (n+1))) else helper (n+1)
    in lazy (helper 1)
