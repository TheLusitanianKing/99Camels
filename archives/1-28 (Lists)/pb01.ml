let rec last l = match l with
	| [] -> None
	| [x] -> Some x
	| t::q -> last q;;

assert (last ["a";"b";"c";"d"] = Some "d");;
assert (last [] = None);;
