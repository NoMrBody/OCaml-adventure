# Tree Data Structure Implementation

## Problem Description

This project involves implementing a set of functions to manipulate a custom tree data structure in OCaml. The tree is defined as follows:

### Tree Datatype Definition
```ocaml
type 'a tree =
  | Leaf of 'a
  | One of int * 'a tree
  | Two of int * int * 'a tree * 'a tree

    Leaf holds a single value.
    One holds an integer representing the size of the subtree and a single subtree.
    Two holds two integers representing the sizes of the left and right subtrees, respectively, and two subtrees.

Example Tree

let tr = Two (4, 1,
              Two (2, 2,
                   Two (1, 1, 
                        Leaf "a", 
                        Leaf "b"),
                   Two (1, 1, 
                        Leaf "c", 
                        Leaf "d")
              ),
              One (1, Leaf "e"))

This tree can be visualized as follows:

                    Two (4, 1)
                  /            \
           Two (2, 2)        One (1)
         /         \             |
    Two (1, 1)  Two (1, 1)    Leaf "e"
   /       \   /       \
Leaf "a" Leaf "b" Leaf "c" Leaf "d"

Functions to Implement
1. search

The search function retrieves the value contained in the ith leaf where counting starts from 0.

    val search : 'a tree -> int -> 'a option

Example:
search tr 2 = Some "c"
search tr 6 = None

2. update

The update function returns the tree obtained from t by replacing the contents of the ith leaf with x.

    val update : 'a tree -> int -> 'a -> bool * 'a tree

Example:
update tr 2 "z" = (true, Two (4, 1, 
                        Two (2, 2,
                             Two (1, 1, 
                                  Leaf "a", 
                                  Leaf "b"),
                             Two (1, 1, 
                                  Leaf "z", 
                                  Leaf "d")
                        ),
                        One (1, Leaf "e")))

3. insert

The insert function inserts a new element at the specified position in the tree.

    val insert : 'a tree -> int -> 'a -> 'a tree

insert tr 2 "z" = Two (5, 1, 
                       Two (3, 2, 
                            Two (1, 1, 
                                 Leaf "a", 
                                 Leaf "z"), 
                            Two (1, 1, 
                                 Leaf "b", 
                                 Leaf "d")
                       ), 
                       One (1, Leaf "e"))

4. remove

The remove function removes the ith element from the tree.

    val remove : 'a tree -> int -> 'a * 'a tree option

remove tr 2 = ("c", Some (Two (3, 1,
                        Two (2, 2, 
                             Two (1, 1, 
                                  Leaf "a", 
                                  Leaf "b"), 
                             Leaf "d"),
                        One (1, Leaf "e"))))