let rec last_two l = match l with
	| [] -> None
	| [x] -> None
	| [x;y] -> Some (x,y)
	| t::q -> last_two q;;
