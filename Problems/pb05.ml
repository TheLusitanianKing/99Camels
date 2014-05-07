let reverse l =
	let rec local acc l = match l with
	| [] -> acc
	| t::q -> local (t::acc) q
	in local [] l;;
