(* Using split from the 17th problem *)

let rotate l n =
	if (n<0) then let nn = (List.length l + n) in
	let a,b = split l nn in (List.append b a)
	else let a,b = split l n in (List.append b a);;

assert (rotate ["a";"b";"c";"d";"e";"f";"g";"h"] 3 = ["d";"e";"f";"g";"h";"a";"b";"c"]);;
assert (rotate ["a";"b";"c";"d";"e";"f";"g";"h"] (-2) = ["g";"h";"a";"b";"c";"d";"e";"f"]);;
