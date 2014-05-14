let is_prime n =
	let rec is_divisor d = 
		(n <> 1) && (d * d > n || (n mod d <> 0 && is_divisor (d+1)))
	in is_divisor 2;;

assert(not(is_prime 1));;
assert(is_prime 7);;
assert(not(is_prime 12));;
