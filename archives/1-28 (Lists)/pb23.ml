let rand_select l n =
	let rec extract_rand acc n l len = match l with
		| [] -> failwith ("tried to access a wrong position")
		| t::q when (n=0) -> (t,List.append acc q)
		| t::q -> extract_rand (t::acc) (n-1) l len
	in
		let rec local n acc l len = match n with
			| 0 -> acc
			| n -> let a,b = extract_rand [] (Random.int len) l len
						in local (n-1) (a::acc) b (len-1)
		in local (min n (List.length l)) [] l n;;
