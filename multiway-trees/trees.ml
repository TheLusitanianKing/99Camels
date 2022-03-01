type 'a mult_tree = T of 'a * 'a mult_tree list;;

let () =
  print_string @@ "Everything is working fine" ^ "\n"