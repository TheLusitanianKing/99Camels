let is_palindrome l =
	l = reverse l;;

assert (is_palindrome ["x";"a";"m";"a";"x"]);;
assert (not (is_palindrome ["a";"b"]));;
