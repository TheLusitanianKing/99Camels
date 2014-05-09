let drop l n =
	let rec local aux l x = match l,x with
		| [],_ -> aux
		| t::q,x when (x=1) -> local aux q n
		| t::q,x -> local (t::aux) q (x-1)
	in List.rev (local [] l n);;

assert (drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3 = ["a";"b";"d";"e";"g";"h";"j"]);;
