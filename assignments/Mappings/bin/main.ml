(* [(1,2); (2,3); (4,6)] *)

let f x = match x with
  | 1 -> 2
  | 2 -> 3
  | 4 -> 6
  | _ -> failwith "no key exists"

let get m x = m x ;;

let put m k v = fun x -> if x = k then v else m x

let f = put (fun x-> failwith "no key exists") 1 2
let f = put f 2 3
let f = put f 3 4

let remove m k = fun x -> if x = k then failwith "no key exists" else m x
