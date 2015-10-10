#use "pb29.ml";;
open List;;

let rec give_me_all_primes_boy a b =
	if a > b then []
	else
		let rest = give_me_all_primes_boy (a+1) b in
		if is_prime a then a::rest else rest;;

assert(length(give_me_all_primes_boy 2 7920) = 1000);;
