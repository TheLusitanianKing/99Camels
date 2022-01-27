(* 31: determine whether a given integer number is prime. *)
let is_prime n =
  let rec helper d = 
    (n <> 1) && (d * d > n || (n mod d <> 0 && helper (d + 1)))
  in helper 2

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

(* 36: determine the prime factors of a given positive integer (2). *)
let factors' n =
  let rec helper d n =
    if n = 1 then []
    else
      if n mod d = 0
        then
          match helper d (n / d) with
          | (x, y)::xs when x = d -> (x, y + 1)::xs
          | xs -> (d, 1)::xs
        else helper (d + 1) n
  in helper 2 n

(* 37: calculate Euler's totient function φ(m) (improved). *)
let power x p =
  let rec helper acc p' =
    if p' < 1 then acc else helper (x * acc) (p' - 1) 
  in helper 1 p

let phi_improved n =
  let fs = factors' n in
	let rec helper acc l = match l with
		| [] -> acc
		| (x, y)::xs -> helper ((x-1) * power x (y-1) * acc) xs
	in helper 1 fs

(* 38: compare the two methods of calculating Euler's totient function. *)
let timeit f a =
  let t1 = Unix.gettimeofday() in ignore(f a);
  let t2 = Unix.gettimeofday() in t2 -. t1

(* 39: a list of prime numbers. *)
let range x y = (* coming from one of the former challenges *)
  let rec helper acc i =
    if i == y then (List.rev acc) @ [i] 
    else helper (i::acc) (if x <= y then i + 1 else i - 1)
  in helper [] x

let all_primes x y = range x y |> List.filter is_prime

(* 40: Goldbach's conjecture. *)
let goldbach x =
	let rec helper d =
		if is_prime d && is_prime (x - d)
      then (d, x - d)
		  else helper (d + 1)
	in helper 2

(* 41: a list of Goldbach compositions. *)
let goldbach_list x y = range x y
  |> List.filter (fun x -> x mod 2 = 0)
  |> List.map (fun x -> (x, goldbach x))

let goldbach_limit x y limit = goldbach_list x y
  |> List.filter (fun (_, (a, b)) -> a > limit && b > limit)

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

  assert (factors' 315 = [(3, 2); (5, 1); (7, 1)]);

  assert (phi_improved 10 = 4);
  assert (phi_improved 13 = 12);

  assert (timeit phi 10090 > timeit phi_improved 10090);

  assert (List.length (all_primes 2 7920) = 1000);

  assert (goldbach 28 = (5, 23));

  let expected =
    [ (10, (3, 7))
    ; (12, (5, 7))
    ; (14, (3, 11))
    ; (16, (3, 13))
    ; (18, (5, 13))
    ; (20, (3, 17))
    ] in
  assert (goldbach_list 9 20 = expected);
  let expected =
    [ (992, (73, 919))
    ; (1382, (61, 1321))
    ; (1856, (67, 1789))
    ; (1928, (61, 1867))
    ] in
  assert (goldbach_limit 1 2000 50 = expected);

  print_string @@ "Everything is working fine" ^ "\n"