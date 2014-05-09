type 'a rle =
	| One of 'a
	| Many of int * 'a;;

let encode l =
	let rec local count acc l = match l with
	| [] -> []
	| [x] when (count=0) -> One x::acc
	| [x] -> Many (count+1,x)::acc (* when count <> 0*)
	| t::(t'::_ as q) when (t=t') -> local (count+1) acc q
	| t::(t'::_ as q) when (count=0) -> local 0 (One t::acc) q (* when t <> t' *)
	| t::(t'::_ as q) -> local 0 (Many (count+1,t)::acc) q (* when t <> t' and count <> 0 *)
	in List.rev (local 0 [] l);;

assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]);;
