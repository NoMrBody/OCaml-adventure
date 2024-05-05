type nat = Zero | Succ of nat
let rec int_to_nat x = match x with
| 0 -> Zero
| _ -> Succ(int_to_nat(x-1))

let rec nat_to_int x = match x with
| Zero -> 0
| Succ(n) -> 1 + nat_to_int n

let rec add x y =
  let degrade n = match n with
  | Zero -> Zero
  | Succ(t) -> t
in match y with 
| Zero -> x
| _ -> add (Succ(x)) (degrade y)


let rec mul x y = match x with 
| Zero -> Zero
| Succ(n) -> add(mul n y) y


let rec power x y = match y with
| Zero -> Succ(Zero)
| Succ(n) -> mul(add n y) x   


let rec leq x y = match x, y with 
| Succ(x), Succ(y) -> leq x y
| _ -> x=Zero


