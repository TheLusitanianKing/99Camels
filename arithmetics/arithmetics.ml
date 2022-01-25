(* 31: determine whether a given integer number is prime. *)
let range x y = (* coming from one of the former challenges *)
  let rec helper acc i =
    if i == y then (List.rev acc) @ [i] 
    else helper (i::acc) (if x <= y then i + 1 else i - 1)
  in helper [] x

let all p ls =
  List.map p ls
  |> List.filter ((=) false)
  |> List.length
  |> ((=) 0)

let is_prime x = (* naive approach *)
  if x <= 1 then false
  else all (fun y -> not (x mod y == 0)) (range 2 (x - 1))

(* list of assertions to test previously defined functions *)
let () =
  assert (not (is_prime 1));
  assert (is_prime 7);
  assert (not (is_prime 12));

  print_string @@ "Everything is working fine" ^ "\n"