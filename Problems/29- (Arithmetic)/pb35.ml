#use "pb34.ml";;

let rec power x p =
	if p<1 then 1
	else x * power x (p-1);;

let phi_pp n =
	let rec local acc l = match l with
		| [] -> acc
		| (a,b)::q -> local ((a-1) * power a (b-1) * acc) q
	in local 1 (factors n);;

assert(phi_pp 10 = 4);;
assert(phi_pp 13 = 12);;
