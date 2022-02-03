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

let () =
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

  print_string @@ "Everything is working fine" ^ "\n"