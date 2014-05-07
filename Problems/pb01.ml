let rec last l = match l with
	| [] -> None
	| [x] -> Some x
	| t::q -> last q;;
