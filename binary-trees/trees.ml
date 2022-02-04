type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree

(* 56: symmetric binary trees. *)
let rec is_mirror t1 t2 = match (t1, t2) with
  | (Empty, Empty) -> true
  | (Node (_, l, r), Node (_, l', r')) ->
    is_mirror l r' && is_mirror r l'
  | _ -> false

let is_symmetric t = match t with
  | Empty -> true
  | Node (_, l, r) -> is_mirror l r

(* 57: binary search trees (dictionaries). *)
let rec insert_into_bst t x = match t with
  | Empty                      -> Node (x, Empty, Empty)
  | Node (y, l, r) when x <= y -> Node (y, insert_into_bst l x, r)
  | Node (y, l, r)             -> Node (y, l, insert_into_bst r x)

let construct = List.fold_left insert_into_bst Empty

(* 61: count the leaves of a binary tree. *)
let rec count_leaves t = match t with
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> count_leaves l + count_leaves r

(* 61A: collect the leaves of a binary tree in a list. *)
let leaves t =
  let rec helper acc t' = match t' with
    | Empty -> acc
    | Node (x, Empty, Empty) -> x :: acc
    | Node (_, l, r) -> helper (helper acc r) l
  in helper [] t

(* 62: collect the internal nodes of a binary tree in a list. *)
let internals t = 
  let rec helper acc t' = match t' with
    | Empty -> acc
    | Node (_, Empty, Empty) -> acc
    | Node (x, l, r) -> helper (x :: helper acc r) l 
  in helper [] t

let () =
  let example_tree =
    Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
       Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty))) in

  let t1 = Node ('x', Empty, Empty) in
  let t2 = Node ('y', Empty, Empty) in
  assert (is_mirror t1 t2);

  let t1 = Node ('x', Node ('y', Empty, Empty), Empty) in
  let t2 = Node ('y', Empty, Empty) in
  assert (not @@ is_mirror t1 t2);

  let t1 = Node ('x', Node ('y', Empty, Empty), Empty) in
  let t2 = Node ('y', Empty, Node ('y', Empty, Empty)) in
  assert (is_mirror t1 t2);

  let t = Node ('x', Node ('y', Empty, Empty), Empty) in
  assert (not @@ is_symmetric t);

  let t = Node ('t', Node ('z', Empty, Empty), Node ('w', Empty, Empty)) in
  assert (is_symmetric t);

  let expected =
    Node
      (3,
      Node (2, Node (1, Empty, Empty), Empty),
      Node (5, Empty, Node (7, Empty, Empty))
      ) in
  assert (construct [3; 2; 5; 7; 1] = expected);

  assert (count_leaves Empty = 0);
  assert (count_leaves example_tree = 3);

  assert (leaves Empty = []);
  assert (leaves example_tree = ['d'; 'e'; 'g']);

  assert (internals (Node ('a', Empty, Empty)) = []);
  assert (internals example_tree = ['b'; 'a'; 'c'; 'f']);

  print_string @@ "Everything is working fine" ^ "\n"