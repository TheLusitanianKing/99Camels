let pack l =
	let rec local i acc l = match l with
	| [] -> []
	| [x] -> (x::i)::acc
	| t::t'::q when (t=t') -> local (t::i) acc (t'::q)
	| t::t'::q -> local [] ((t::i)::acc) (t'::q)
	in local [] [] l;;
