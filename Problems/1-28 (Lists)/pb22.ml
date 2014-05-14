let rec range x y = match x,y with
	| x,y when x=y -> [x]
	| x,y when x>y -> x::range (x-1) y
	| x,y -> x::range (x+1) y;;

assert (range 4 9 = [4;5;6;7;8;9]);;
assert (range 9 4 = [9;8;7;6;5;4]);;
