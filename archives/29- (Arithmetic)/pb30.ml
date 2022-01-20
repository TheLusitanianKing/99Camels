let rec pgcd x y = match y with
	| 0 -> x
	| _ -> pgcd y (x mod y);;

assert(pgcd 13 27 = 1);;
assert(pgcd 20536 7826 = 2);;
