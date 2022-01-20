let remove_at n l =
	let rec local acc x l = match x,l with
		| _,[] -> acc
		| x,t::q when (x=n) -> local acc (x+1) q
		| x,t::q -> local (t::acc) (x+1) q
	in List.rev (local [] 0 l);;

assert (remove_at 1 ["a";"b";"c";"d"] = ["a";"c";"d"]);;
