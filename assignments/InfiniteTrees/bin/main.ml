type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree)

let rec layer_tree r = LNode (r, (fun () -> layer_tree(r+1)), (fun () -> layer_tree(r+1)))

let rec interval_tree l h =
  LNode ((l, h), (fun () -> interval_tree l ((l+.h)/.2.)), (fun () -> interval_tree l ((l+.h)/.2.)))

let rec rational_tree n d = 
  LNode ((n, d), (fun () -> rational_tree n (d+1)), (fun () -> rational_tree n (d+1)))
