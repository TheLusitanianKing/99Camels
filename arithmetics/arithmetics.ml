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

(* 32: determine the greatest common divisor of two positive integer numbers. *)
let rec gcd x y = match y with
	| 0 -> x
	| _ -> gcd y (x mod y)

(* 33: determine whether two positive integer numbers are coprime. *)
let coprime x y = gcd x y = 1

(* 34: calculate Euler's totient function φ(m). *)
let phi m =
	let rec helper acc x = match x with
    | x when x >= m -> acc
		| x when coprime x m -> helper (acc + 1) (x + 1)
		| _ -> helper acc (x + 1)
	in helper 1 2

(* 35: determine the prime factors of a given positive integer. *)
let factors n =
	let rec helper acc d n =
    if n = 1 then acc
    else
      if n mod d = 0
        then helper (d::acc) d (n / d)
        else helper acc (d + 1) n
	in List.rev @@ helper [] 2 n

(* list of assertions to test previously defined functions *)
let () =
  assert (not @@ is_prime 1);
  assert (is_prime 7);
  assert (not @@ is_prime 12);

  assert (gcd 13 27 = 1);
  assert (gcd 20536 7826 = 2);

  assert (coprime 13 27);
  assert (not @@ coprime 20536 7826);

  assert (phi 1 = 1);
  assert (phi 10 = 4);
  assert (phi 13 = 12);

  assert (factors 315 = [3; 3; 5; 7]);

  print_string @@ "Everything is working fine" ^ "\n"