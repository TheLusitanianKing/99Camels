#use "pb30.ml";;

let coprime x y =
	(pgcd x y = 1);;

assert(coprime 13 27);;
assert(not(coprime 20536 7826));;
