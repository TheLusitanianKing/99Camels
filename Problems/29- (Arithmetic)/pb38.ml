#use "pb29.ml";;

let rec goldbachova_hypoteza x =
	let rec aux d =
		if is_prime d && is_prime (x-d) then (d, x-d)
		else aux (d+1)
	in aux 2;;

assert((goldbachova_hypoteza 28) = (5, 23));;

