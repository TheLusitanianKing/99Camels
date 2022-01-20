let factors n =
	let rec local d n = match n with
		| 1 -> []
		| n when (n mod d = 0) ->
			(match local d (n/d) with
				| (t,n)::q when t=d -> (t,n+1)::q
				| l -> (d,1)::l)
		| _ -> local (d+1) n
	in local 2 n;;

assert (factors 315 = [(3,2);(5,1);(7,1)]);;
