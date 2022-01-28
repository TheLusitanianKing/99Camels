type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

(* 46 & 47 & 48: truth tables for logical expressions. *)
(* The next two functions aren't tail recursive *)
(* There is probably a better solution *)
let table_possibilities vars =
  let rec helper acc vs = match vs with
    | [] -> [List.rev acc]
    | (x::xs) -> helper ((x, true) :: acc) xs @ helper ((x, false) :: acc) xs
  in helper [] vars

let rec eval vars expr = match expr with
  | Var x           -> List.assoc x vars
  | Not exp         -> not (eval vars exp)
  | And (exp, exp') -> eval vars exp && eval vars exp'
  | Or (exp, exp')  -> eval vars exp || eval vars exp'

let table vars expr =
  let tp = table_possibilities vars in
  List.map (fun tp -> (tp, eval tp expr)) tp

let table2 x y expr =
  let t = table [x; y] expr in
  let f tt = match tt with
    | ([(a, ar); (b, br)], r) when (a=x && b=y) -> (ar, br, r)
    | _ -> failwith "Something has gone wrong." in
  List.map f t

(* 49: Gray code. *)
(* also called "reflected binary" *)
let gray n =
  if n < 1 then failwith "Cannot get code with n < 1."
  else
    let rec helper current_code m =
      if m = n then current_code
      else
        let current_code_rev = List.rev current_code in
        let code' =
          (List.map (fun x -> "0" ^ x) current_code) @
          (List.map (fun x -> "1" ^ x) current_code_rev) in
        helper (code') (m + 1)
    in helper ["0"; "1"] 1

let () =
  let truth_table =
    [ (true,  true,  true)
    ; (true,  false, true)
    ; (false, true,  false)
    ; (false, false, false)
    ] in
  assert (table2 "a" "b" (And (Var "a", Or (Var "a", Var "b"))) = truth_table);
  
  let truth_table =
    [ ([("a", true);  ("b", true)],  true)
    ; ([("a", true);  ("b", false)], true)
    ; ([("a", false); ("b", true)],  false)
    ; ([("a", false); ("b", false)], false)
    ] in
  assert (table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b"))) = truth_table);
  
  let call =
    let a = Var "a" and b = Var "b" and c = Var "c" in
    table ["a"; "b"; "c"] (Or (And (a, Or (b,c)), Or (And (a,b), And (a,c)))) in
  let truth_table =
    [ ([("a", true);  ("b", true);  ("c", true)],  true)
    ; ([("a", true);  ("b", true);  ("c", false)], true)
    ; ([("a", true);  ("b", false); ("c", true)],  true)
    ; ([("a", true);  ("b", false); ("c", false)], false)
    ; ([("a", false); ("b", true);  ("c", true)],  false)
    ; ([("a", false); ("b", true);  ("c", false)], false)
    ; ([("a", false); ("b", false); ("c", true)],  false)
    ; ([("a", false); ("b", false); ("c", false)], false)
    ] in
  assert (call = truth_table);

  assert (gray 1 = ["0"; "1"]);
  assert (gray 2 = ["00"; "01"; "11"; "10"]);
  assert (gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]);

  print_string @@ "Everything is working fine" ^ "\n"