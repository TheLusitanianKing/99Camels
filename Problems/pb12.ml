type 'a rle =
	| One of 'a
	| Many of int * 'a;;

let decode l =
	let rec decode_r rle acc = match rle with
	| One x -> x::acc
	| Many (1,x) -> x::acc
	| Many (n,x) -> decode_r (Many (n-1,x)) (x::acc)
	in
	let rec local acc l = match l with
	| [] -> acc
	| t::q -> local (List.append (decode_r t []) acc) q
	in List.rev (local [] l);;

assert (decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")] = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]);;
