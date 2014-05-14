#use "pb31.ml";;

let phi m =
	let rec local acc d = match d with
		| d when (d<m && coprime d m) -> local (acc+1) (d+1)
		| d when (d<m && not(coprime d m)) -> local acc (d+1)
		| _ -> acc
	in local 0 1;;

assert(phi 10 = 4);;
assert(phi 13 = 12);;
