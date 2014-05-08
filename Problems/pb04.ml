let rec length l = match l with
	| [] -> 0
	| t::q -> 1+length q;;

let length_tail_recursive l =
	let rec local aux l = match l with
	| [] -> aux
	| t::q -> local (aux+1) q
	in local 0 l;;

assert (length ["a";"b";"c"] = 3);;
assert (length [] = 0);;

assert (length_tail_recursive ["a";"b";"c"] = 3);;
assert (length_tail_recursive [] = 0);;
