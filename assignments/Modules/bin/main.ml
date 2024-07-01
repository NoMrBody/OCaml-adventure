module type Ring = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val mul : t -> t -> t
  val compare : t -> t -> int
  val to_string : t -> string
end

module IntRing : Ring with type t = int = struct
  type t = int
  let zero = 0
  let one = 1
  let add = (+)
  let mul = ( * )
  let compare = compare
  let to_string = string_of_int
end

module FloatRing : Ring with type t = float = struct
  type t = float
  let zero = 0.
  let one = 1.
  let add = ( +. )
  let mul = ( *. )
  let compare = compare
  let to_string = string_of_float
end

module type FiniteRing = sig
  include Ring
  val elems : t list
end

module Boolring : FiniteRing with type t = bool = struct
  type t = bool
  let zero = false
  let one = true
  let add = ( <> ) (* XOR *)
  let mul = (&&) (* AND *)
  let compare = compare
  let to_string = string_of_bool
  let elems = [true; false]
end

module SetRing (D : FiniteRing) : Ring with type t = D.t list = struct
  type t = D.t list
  let zero = []
  let one = D.elems
  let add l1 l2 = List.fold_left (fun a h -> if List.exists (fun x -> D.compare x h = 0) a then a else h::a) l1 l2
  let mul l1 l2 = List.fold_left (fun a h -> if List.exists (fun x -> D.compare x h = 0) l1 then h::a else a) [] l2
  let compare l1 l2 = 
  let rec compare_aux a b = match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x::xs, y::ys -> let c = D.compare x y in if c = 0 then compare_aux xs ys else c
  in compare_aux (List.sort_uniq D.compare l1) (List.sort_uniq D.compare l2)
  let to_string l = List.fold_left (fun a h -> D.to_string h ^ " " ^ a) "" l
end