(* Load Unix module *)
#load "unix.cma";;
open Unix;;

(* Load phi and phi_pp *)
#use "pb32.ml";;
#use "pb35.ml";;

let please_time_me f a =
	let t = Unix.gettimeofday() in
		ignore(f a);
	let t' = Unix.gettimeofday() in
		t' -. t;;

assert(please_time_me phi 10090 > please_time_me phi_pp 10090);;
