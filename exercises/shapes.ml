type point = float * float

type shape =  Circle of {center : point; radius : float}
| Rectangle of {left_bottom : point; right_up: point;}

let average p1 p2 = (p1 +. p2) /. 2.

let center s = match s with
| Circle {center; radius} -> center
| Rectangle {left_bottom; right_up} -> 
  let (x_lb, y_lb) = left_bottom in 
  let (x_ur, y_ur) = right_up in
  (average x_lb x_ur, average y_lb y_ur) 

(* actually, this function doesn't modify the position of the object *)
(* after calling the move function new object is created on the desired position, 
 yet the position of the initial object is left unchanged *)
(*I wrote this just for fun :) for fun X[ *)
let move obj (dx : float) (dy : float ) = match obj with
| Circle {center; radius} -> Circle {center = (fst center +. dx, fst center +. dy); radius}
| Rectangle {left_bottom; right_up} -> 
  let (x_lb, y_lb) = left_bottom in
  let (x_ur, y_ur) = right_up in
  Rectangle{left_bottom = (x_lb +. dx, y_lb +. dy);right_up = (x_ur +. dx, y_ur +. dy)}

let c1 = Circle {center=(0., 0.); radius=2.5}
let r1 = Rectangle {left_bottom = (-1., -1.); right_up = (2.,2.)}