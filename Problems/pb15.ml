let replicate l n =
	let rec create_list elem n acc = match n with
		| 0 -> acc
		| n -> create_list elem (n-1) (elem::acc)
	in
	let rec local acc l = match l with
		| [] -> acc
		| t::q -> local (List.append (create_list t n []) acc) q
	in List.rev (local [] l);;

assert (replicate ["a";"b";"c"] 2 = ["a";"a";"b";"b";"c";"c"]);;
assert (replicate ["a"] 4 = ["a";"a";"a";"a"]);;
assert (replicate ["a";"b";"c";"d"] 0 = []);;
assert (replicate ["a";"b"] 1 = ["a";"b"]);;
