type quadtree_node = NoPoint | Point of int*int 
| QNode of quadtree_node * quadtree_node * quadtree_node * quadtree_node

type quadtree = {
  root : quadtree_node;
  height : int;
  width : int;
}

let rec insert (px, py) qtree= 
  let rec impl (x1, y1, x2, y2) (px, py) qnode =
    let xmid  = (x1+x2)/2 in
    let ymid  = (y1+y2)/2 in 
    match qnode with 
      | NoPoint -> Point (px, py)
      | QNode (nn, np, pn, pp) -> (match (px<xmid, py<ymid) with
          | true, true -> QNode(impl(x1, y1, xmid, ymid) (px, py) nn, np, pn, pp)
          | true, false -> QNode(nn, impl(x1, ymid, xmid, y2)(px, py)np, pn, pp)
          | false, true -> QNode(nn, np, impl(xmid, y1, x2, ymid)(px, py)pn, pp)
          | false, false -> QNode(nn, np, pn, impl(xmid, ymid, x2, y2)(px, py)pp))
      | Point (pxp, pyp) -> if ((px, py)=(pxp, pyp)) then Point(px, py) 
        else impl(x1, y1, x2, y2)(px, py)(impl(x1, y1, x2, y2)(pxp, pyp)(QNode(NoPoint, NoPoint, NoPoint, NoPoint)))
  in impl (0, 0, qtree.width, qtree.height) (px, py) qtree.root
