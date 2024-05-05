let foo x y b = 
  let rec func x y b = match (x<y) with 
    | true -> if b then func (x+1) y false else func x (y-1) true
    | false -> x
  in 
  match x>y with 
  | true -> func y x b
  | false -> func x y b 