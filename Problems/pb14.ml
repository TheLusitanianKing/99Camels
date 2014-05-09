let duplicate l =
	let rec local acc l = match l with
	| [] -> acc
	| t::q -> local (List.append [t;t] acc) q
	in List.rev (local [] l);;

assert (duplicate ["a";"b";"c";"c";"d"] = ["a";"a";"b";"b";"c";"c";"c";"c";"d";"d"]);;
