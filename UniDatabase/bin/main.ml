type student = {
  first_name : string;
  last_name : string;
  id : int;
  grades : (int * float) list;
  semester : int;
}

type database = student list

let insert st db = st::db

let rec find_by_id i db = match db with
| [] -> []
| h::t -> if h.id=i then [h] else find_by_id i t


let rec find_by_lastname lname db = match db with
| [] -> []
| h::t -> if h.last_name = lname then h::find_by_lastname lname t else find_by_lastname lname t


let rec remove_by_id id db = match db with
| [] -> []
| h::t -> if h.id = id then remove_by_id id t else h::remove_by_id id t

let rec count_in_semester int db = 
  let rec helper int db acc = match db with
  | [] -> acc
  | h::t -> if h.semester = int then helper int t acc+1 else helper int t acc
in helper int db 0

let rec average_grade id db = match db with
  | [] -> 0.
  | h::t -> if h.id = id then 
    let rec sum_grades grades = match grades with
    | [] -> 0.0
    | (_, value)::t -> value +. sum_grades t
  in let rec count_grades grades = match grades with
    | [] -> 0.0
    | (number, _)::t -> 1. +. sum_grades t
  in let (sum, count) = (sum_grades h.grades, count_grades h.grades) in
  if count = 0.0 then 0.0 else sum /. count
else average_grade id t


