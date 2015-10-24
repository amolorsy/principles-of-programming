exception TODO

type matcher =
	| Empty
  	| Char of int
	| Append of (matcher * matcher)
	| Union of (matcher * matcher)
	| Star of matcher

type string = int list

let rec union l r =     
    match l, r with
	| [], _ -> r
	| _, [] -> l
	| hl::tl, hr::tr -> 
		if hl < hr then hl::(union tl r)
		else if hl = hr then hl::(union tl tr)
		else hr::(union l tr)

(* See Figure 1 (bottom) *)
let rec dagger (m: matcher) (char: int) : matcher list =
	match m with
	| Empty -> []
	| Char x -> if x = char then [Empty] else []
	| Append (l, r) ->
		let l_prime = (dagger l char) in
			List.map (fun x -> Append (x, r)) l_prime
	| Union (l, r) -> union (dagger l char) (dagger r char)
	| Star x ->
		let x_prime = (dagger x char) in
			List.map (fun i -> Append (i, Star x)) x_prime

(* See Figure 1 (top) *)
let rec match_string (m: matcher) (s: string) : bool =
	let rec iter_until_true l_lst r_m str =
		match l_lst with
		| [] -> false
		| h::t ->
			let value = (match_string (Append (h, r_m)) str) in
				if value = true then true else (iter_until_true t r_m str) in
	match m with
	| Empty -> s = []
	| Char x -> s = [x]
	| Append (l, r) ->
		(match s with
		| [] -> (match_string l []) && (match_string r []) 
		| h::t ->
			let l_prime = (dagger l h) in
				(print_endline (string_of_int (List.length l_prime)));
				(match l_prime with
				| [] -> (match_string r s) && (match_string l [])
				| _ ->
					let fst = (iter_until_true l_prime r t) in
						if fst = true then true else fst || ((match_string r s) && (match_string l []))))
	| Union (l, r) -> (match_string l s) || (match_string r s)
	| Star x ->
		(match s with
		| [] -> true
		| h::t ->
			let x_prime = (dagger x h) in
				(match x_prime with
				| [] -> false
				| _ -> (iter_until_true x_prime (Star x) t)))
		
		
let _ = print_endline (string_of_bool (match_string (Append (Char 3, Char 4)) [4])) (* true *);;
