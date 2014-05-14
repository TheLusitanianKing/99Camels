let slice l i j =
	let rec local acc l n = match l,n with
		| [],_ -> acc
		| t::q,n when (n>=i && n<=j) -> local (t::acc) q (n+1)
		| t::q,n -> local acc q (n+1)
	in List.rev (local [] l 0);;

assert (slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6 = ["c";"d";"e";"f";"g"]);;
