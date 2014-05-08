type 'a node =
	| One of 'a
	| Many of 'a node list;;

let rec flatten ln = match ln with
	| [] -> []
	| One x::q -> x::flatten q
	| Many x::q -> List.append (flatten x) (flatten q);;

assert (flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]] = ["a";"b";"c";"d";"e"]);;
