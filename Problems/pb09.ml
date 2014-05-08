let pack l =
	let rec local i acc l = match l with
	| [] -> []
	| [x] -> (x::i)::acc
	| t::t'::q when (t=t') -> local (t::i) acc (t'::q)
	| t::t'::q -> local [] ((t::i)::acc) (t'::q)
	in List.rev (local [] [] l);;

assert (pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"] = [["a";"a";"a";"a"];["b"];["c";"c"];["a";"a"];["d";"d"];["e";"e";"e";"e"]]);;
