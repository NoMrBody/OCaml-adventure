type 'a llist = Cons of 'a * (unit -> 'a llist)

let rec lnat n = Cons(n, (fun () -> lnat(n+1)))

let rec lfib () = 
  let rec helper a b = Cons(a, (fun () -> helper b (a+b)))
in helper 0 1

let rec ltake n (Cons(h, t)) = 
  if n<=0 then [] else h::ltake (n-1) (t ())

let rec lfilter f (Cons(h, t)) = if f h = true then Cons(h, fun () -> lfilter f (t ())) 
  else lfilter f (t ())