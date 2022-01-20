#use "pb22.ml";;
#use "pb23.ml";;

(* Draw n different random numbers from the set 1..m *)
let lotto_select n m =
	rand_select (range 1 m) n;;
