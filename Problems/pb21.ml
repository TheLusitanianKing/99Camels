let insert_at elem n l =
	let rec local acc li pos = match pos,li with
		| p,[] when (n >= List.length l) -> (elem::acc)
		| p,[] -> acc
		| p,l when (p=n) -> local (elem::acc) l (pos+1)
		| p,t::q when (p<>n) -> local (t::acc) q (pos+1)
	in List.rev (local [] l 0);;

let rec insert_at_2 elem n l = match l with
	| [] -> [elem]
	| (t::q as l) when (n=0) -> elem::l
	| t::q -> t::insert_at_2 elem (n-1) q;;

assert (insert_at "alfa" 1 ["a";"b";"c";"d"] = ["a";"alfa";"b";"c";"d"]);;
assert (insert_at "alfa" 3 ["a";"b";"c";"d"] = ["a";"b";"c";"alfa";"d"]);;
assert (insert_at "alfa" 4 ["a";"b";"c";"d"] = ["a";"b";"c";"d";"alfa"]);;
assert (insert_at "alfa" 10 ["a";"b";"c";"d"] = ["a";"b";"c";"d";"alfa"]);;

assert (insert_at_2 "alfa" 1 ["a";"b";"c";"d"] = ["a";"alfa";"b";"c";"d"]);;
assert (insert_at_2 "alfa" 10 ["a";"b";"c";"d"] = ["a";"b";"c";"d";"alfa"]);;
