open List;;

(* 1: write a function that returns the last element of a list. *)
let rec last ls = match ls with
  | [] -> None
  | [x] -> Some x
  | (_::xs) -> last xs

(* 2: find the last but one (last and penultimate) elements of a list. *)
let rec last_two ls = match ls with
  | [] -> None
  | [_] -> None
  | [x; y] -> Some (x, y)
  | (_::xs) -> last_two xs

(* 3: find the K'th element of a list. *)
let rec at n ls =
  if n < 1 then failwith "Indexes start at 1."
  else
    match ls with
    | [] -> None
    | (x::xs) ->
        if n = 1 then Some x
        else at (n - 1) xs

(* 4: find the number of elements of a list. *)
let length l =
  let rec helper acc l' = match l' with
    | [] -> acc
    | (_::xs) -> helper (acc + 1) xs
  in helper 0 l

(* 5: reverse a list. *)
let rev l = List.fold_left (fun acc x -> x::acc) [] l

(* 6: find out whether a list is a palindrome. *)
let is_palindrome ls = (rev ls) = ls

(* 7: flatten a nested list structure. *)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let rec flatten ns =
  match ns with
  | [] -> []
  | (One x::xs) -> x :: flatten xs
  | (Many ns'::xs) -> List.concat [flatten ns'; flatten xs]

(* 8: eliminate consecutive duplicates of list elements. *)
let compress ls =
  let rec helper acc ls' = match ls' with
    | [] -> acc
    | [x] -> rev (x::acc)
    | x::x'::xs when x = x' -> helper acc (x'::xs)
    | x::x'::xs -> helper (x::acc) (x'::xs)
  in helper [] ls

(* 9: pack consecutive duplicates of list elements into sublists. *)
let pack ls =
  let rec helper acc tmp_acc ls' = match ls' with
    | [] -> tmp_acc :: acc
    | [x] -> (x :: tmp_acc) :: acc
    | (x::x'::xs) when x = x' -> helper acc (x :: tmp_acc) (x'::xs)
    | (x::x'::xs) -> helper ((x :: tmp_acc) :: acc) [] (x'::xs)
  in rev (helper [] [] ls)

(* 10: run-length encoding of a list. *)
let encode ls =
  pack ls |> List.map (fun packed -> (length packed, List.hd packed))

(* 11: modified run-length encoding. *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode' ls =
  let f l = match l with
    | [] -> failwith "Should not happen."
    | [x] -> One x
    | ls -> Many (List.length ls, List.hd ls) in
  pack ls |> List.map f

(* 12: decode a run-length encoded list. *)
let replicate n x =
  let rec helper acc n' =
    if n' <= 0 then acc else helper (x::acc) (n'-1)
  in helper [] n

let decode ls =
  let f x = match x with
    | One x -> [x]
    | Many (n, x) -> replicate n x in
  List.map f ls |> List.flatten

(* 13: run-length encoding of a list (direct solution). *)
let partition p ls =
  let rec helper acc ls' = match ls' with
    | (x::xs) when p x -> helper (x :: acc) xs
    | s -> (rev acc, s)
  in helper [] ls

let encode'' ls =
  let rec helper acc ls' = match ls' with
    | [] -> acc
    | (x::_) as s ->
      let (ys, zs) = partition (fun n -> n = x) s in
      let len = length ys in
      let t = if len == 1 then One x else Many (len, x) in
      helper (t::acc) zs
  in rev @@ helper [] ls

(* 14: duplicate the elements of a list. *)
let duplicate ls =
  let rec helper acc ls' = match ls' with
    | [] -> acc
    | (x::xs) -> helper (x::x::acc) xs
  in rev @@ helper [] ls

let () =
  assert (last ["a"; "b"; "c"; "d"] = Some "d");
  assert (last [] = None);

  assert (last_two ["a"; "b"; "c"; "d"] = Some ("c", "d"));
  assert (last_two ["a"] = None);

  assert (at 3 ["a"; "b"; "c"; "d"; "e"] = Some "c");
  assert (at 3 ["a"] = None);

  assert (length ["a"; "b"; "c"] = 3);
  assert (length [] = 0);

  assert (rev ["a"; "b"; "c"] = ["c"; "b"; "a"]);
  assert (rev [1; 2; 3] = [3; 2; 1]);

  assert (is_palindrome ["x"; "a"; "m"; "a"; "x"]);
  assert (not @@ is_palindrome ["a"; "b"]);

  assert (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]
          = ["a"; "b"; "c"; "d"; "e"]);

  assert (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
          = ["a"; "b"; "c"; "a"; "d"; "e"]);

  assert (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
          = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]);
  assert (pack ["a"; "b"; "c"; "d"; "e"] = [["a"]; ["b"]; ["c"]; ["d"]; ["e"]]);

  assert (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
          = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]);
  
  assert (encode' ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
          = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);

  assert (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
          = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]);
        
  assert (encode'' ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
          = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]);
  
  assert (duplicate ["a"; "b"; "c"; "c"; "d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]);

  print_string @@ "Everything is working fine" ^ "\n"