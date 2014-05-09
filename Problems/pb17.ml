let split l n =
	let rec local acc l x = match l,x with
	| [],_ -> let a,b = acc in (List.rev a, List.rev b)
	| t::q,x when (x<=n) -> let a,b = acc in local (t::a,b) q (x+1)
	| t::q,x -> let a,b = acc in local (a,t::b) q (x+1) (* when x>n *)
	in local ([],[]) l 1;;

assert (split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = (["a";"b";"c"],["d";"e";"f";"g";"h";"i";"j"]));;
assert (split ["a";"b";"c"] 4 = (["a";"b";"c"],[]));;
