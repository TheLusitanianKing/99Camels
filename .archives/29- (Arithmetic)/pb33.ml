let factors n =
	let rec local d n = match n with
		| 1 -> []
		| x when (n mod d = 0) -> d::local d (n/d)
		| x -> local (d+1) n
	in local 2 n;;

assert(factors 315 = [3;3;5;7]);;
