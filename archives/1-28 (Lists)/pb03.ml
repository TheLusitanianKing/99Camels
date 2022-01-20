let rec at n l = match n,l with
	| _,[] -> None
	| 1,t::q -> Some t
	| x,t::q -> at (x-1) q;;

assert (at 3 ["a";"b";"c";"d";"e"] = Some "c");;
assert (at 3 ["a"] = None);;
