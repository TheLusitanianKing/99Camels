open List;; (* will only be used for folds *)

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
  let rec helper acc last_seen ls' = match (ls', last_seen) with
    | ([], _) -> rev acc
    | ((x::xs), Some x') when x = x' -> helper acc last_seen xs
    | ((x::xs), _) -> helper (x::acc) (Some x) xs
  in helper [] None ls

(* 9: pack consecutive duplicates of list elements into sublists. *)
let pack ls =
  let rec helper acc tmp_acc last_seen ls' = match (ls', last_seen) with
    | ([], _) -> rev (tmp_acc :: acc)
    | ((x::xs), Some x') when x = x' -> helper acc (x :: tmp_acc) last_seen xs
    | ((x::xs), _) when tmp_acc = [] -> helper acc (x :: tmp_acc) (Some x) xs
    | ((x::xs), _) -> helper (tmp_acc :: acc) [x] (Some x) xs
  in helper [] [] None ls

(* 10: run-length encoding of a list. *)
let encode ls =
  pack ls |> List.map (fun packed -> (length packed, List.hd packed))

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
        
  print_string @@ "Everything is fine" ^ "\n"