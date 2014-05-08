let encode l =
	let rec local current acc l = match current,l with
	| _, [] -> (current::acc)
	| (0,_), t::q -> local (1,t) acc q
	| (x,y), t::q when (y=t) -> local (x+1,y) acc q
	| (x,y), t::q -> local (1,t) (current::acc) q
	in List.rev (local (0,"") [] l);;

assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [(4,"a");(1,"b");(2,"c");(2,"a");(1,"d");(4,"e")]);;
