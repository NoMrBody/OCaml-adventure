type tree = Empty | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop

let rec crawl cmd_list tree = 
  let rec impl cmds stack tr = match cmds, stack, tr with
    | [], s, t -> false, [], s, t
    | Up::cs, ss, t -> true, cs, ss, t
    | Left::cs, ss, Node(v, l, r) -> 
      let u, nc, ns, nt = impl cs ss l in
      if u then impl nc ns (Node(v, nt, r)) else u, nc, ns, Node(v, nt, r)
    | Right::cs, ss, Node(v, l, r) -> 
      let u, nc, ns, nt = impl cs ss r in
      if u then impl nc ns (Node(v, l, nt)) else u, nc, ns, Node(v, l, nt)
    | New v::cs, ss, _ -> impl cs ss (Node(v, Empty, Empty))
    | Delete::cs, ss, _ -> impl cs ss Empty
    | Push::cs, ss, t -> impl cs (t::ss) t
    | Pop::cs, s::ss, _ -> impl cs ss s
    | _ -> failwith "inchident"
  in let _,_,_, t = impl cmd_list [] tree in t