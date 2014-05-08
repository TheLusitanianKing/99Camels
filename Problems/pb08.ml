let rec compress l = match l with
	| [] -> []
	| [x] -> [x]
	| t::t'::q when (t=t') -> compress(t'::q)
	| t::t'::q -> t::compress(t'::q);;

assert (compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a";"b";"c";"a";"d";"e"]);;
