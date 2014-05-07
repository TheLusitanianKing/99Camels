let rec compress l = match l with
	| [] -> []
	| [x] -> [x]
	| t::t'::q when (t=t') -> compress(t'::q)
	| t::t'::q -> t::compress(t'::q);;
