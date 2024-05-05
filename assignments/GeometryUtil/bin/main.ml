
module type GeometryUtilSig = sig
  type point = float * float
  
  val rectangle_area : point -> point -> float
  val rectangle_perimeter : point -> point -> float

  val circle_area : float -> float
  val circle_circumference : float -> float

  val triangle_area : point -> point -> point -> float
  val triangle_perimeter : point -> point -> point -> float

end

module GeometryUtil : GeometryUtilSig = struct
  type shape = Rectangle | Circle | Triangle
  type point = float * float

  let distance (x1 , y1) (x2, y2) = 
    let dx = x2 -. x1 in
    let dy = y2 -. y1 in
    sqrt(dx *. dx +. dy *. dy)

  let rectangle_area (x1, y1) (x2, y2) = 
    let width = abs_float (x2 -. x1) in
    let height = abs_float (y2 -. y1) in
    width *. height
    

  let rectangle_perimeter (x1, y1) (x2, y2) = 
    let width = abs_float (x2 -. x1) in
    let height = abs_float (y2 -. y1) in
    2. *. (width +. height)

  let circle_area radius = 
    let pi = 3.1415 in
    pi *. radius *. radius

  let circle_circumference radius = 
    let pi = 3.1415 in
    pi *. radius *. 2.0

  let triangle_perimeter (a : point) (b : point) (c : point) = 
    let la = distance a b in
    let lb = distance b c in
    let lc = distance a c in
    la +. lb +. lc

  let triangle_area (a:point) (b:point) (c:point) = 
    let pprime = 
      let la = distance a b in
      let lb = distance b c in
      let lc = distance a c in
        (la +. lb +. lc) /. 2.
    in sqrt(pprime *. (pprime -. distance a b) *. (pprime -. distance b c) *. (pprime -. distance a c))
end