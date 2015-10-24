module type SMATCH = sig
  type c =
    | Zero
    | One
    | Two
    | Mult of c * c
    | Sum of c * c
    | Opt of c (* c? *)
    | Star of c (* c* *)
 
  val smatch: string -> c -> bool
end

module Smatch : SMATCH = struct
  exception ETODO

  type c =
    | Zero
    | One
    | Two
    | Mult of c * c
    | Sum of c * c
    | Opt of c (* c? *)
    | Star of c (* c* *)

  type c_imp =
    | Empty
	| Zero_imp
	| One_imp
	| Two_imp
	| Mult_imp of c_imp * c_imp
	| Sum_imp of c_imp * c_imp
	| Opt_imp of c_imp
	| Star_imp of c_imp

  let rec c_to_c_imp =
  	fun x ->
		match x with
		| Zero -> Zero_imp
		| One -> One_imp
		| Two -> Two_imp
		| Mult (l, r) -> Mult_imp (c_to_c_imp l, c_to_c_imp r)
		| Sum (l, r) -> Sum_imp (c_to_c_imp l, c_to_c_imp r)
		| Opt x -> Opt_imp (c_to_c_imp x)
		| Star x -> Star_imp (c_to_c_imp x)

  let rec string_to_char_list s =
  	match s with 
  	| "" -> []
  	| _ -> (String.get s 0)::(string_to_char_list (String.sub s 1 ((String.length s) - 1)))
  
  let rec union l r =     
    match l, r with
	| [], _ -> r
	| _, [] -> l
	| hl::tl, hr::tr -> 
		if hl < hr then hl::(union tl r)
		else if hl = hr then hl::(union tl tr)
		else hr::(union l tr)

  let rec dagger (m: c_imp) (ch: char) : c_imp list =
	match m with
	| Empty -> []
	| Zero_imp -> if ch = '0' then [Empty] else []
	| One_imp -> if ch = '1' then [Empty] else []
	| Two_imp -> if ch = '2' then [Empty] else []
	| Mult_imp (l, r) ->
		(match l with
		| Empty -> (dagger r ch)
        | Zero_imp -> if ch = '0' then [r] else []
	    | One_imp -> if ch = '1' then [r] else []
	    | Two_imp -> if ch = '2' then [r] else []	
		| Mult_imp (l1, l2) -> (dagger (Mult_imp (l1, (Mult_imp (l2, r)))) ch)
		| Sum_imp (l1, l2) -> union (dagger (Mult_imp (l1, r)) ch) (dagger (Mult_imp (l2, r)) ch)
		| Opt_imp x -> union (dagger r ch) (dagger (Mult_imp (x, r)) ch)
		| Star_imp x ->
			let x_prime = (dagger x ch) in
				union (dagger r ch) (List.map (fun k -> (Mult_imp (k, m))) x_prime))
	| Sum_imp (l, r) -> union (dagger l ch) (dagger r ch)
	| Opt_imp x -> union (dagger Empty ch) (dagger x ch)
	| Star_imp x ->
		let x_prime = (dagger x ch) in
			List.map (fun i -> Mult_imp (i, (Star_imp x))) x_prime

  let rec match_string (m: c_imp) (s: char list) : bool =
	let rec iter_until_true l_lst r_m str =
  		(match l_lst with
		| [] -> false
		| h::t ->
			let value = (match_string (Mult_imp (h, r_m)) str) in
				if value = true then value else (iter_until_true t r_m str)) in
	match m with
	| Empty -> s = []
	| Zero_imp -> s = ['0']
	| One_imp -> s = ['1']
	| Two_imp -> s = ['2']
	| Mult_imp (l, r) ->
		(match s with
		| [] -> (match_string l []) && (match_string r [])
		| h::t ->
			(match l with
			| Empty -> (match_string r s)
			| Zero_imp -> if h = '0' then (match_string r t) else false 
	    	| One_imp -> if h = '1' then (match_string r t) else false
			| Two_imp -> if h = '2' then (match_string r t) else false
			| Mult_imp (l1, l2) -> (match_string (Mult_imp (l1, (Mult_imp (l2, r)))) s)
			| Sum_imp (l1, l2) -> (match_string (Mult_imp (l1, r)) s) || (match_string (Mult_imp (l2, r)) s)
			| Opt_imp x -> (match_string r s) || (match_string (Mult_imp (x, r)) s)
			| Star_imp x ->
				 let x_prime = (dagger x h) in
				 	(match_string r s) || (iter_until_true x_prime m t)))
	| Sum_imp (l, r) -> (match_string l s) || (match_string r s)
	| Opt_imp x -> s = [] || (match_string x s)
	| Star_imp x ->
		(match s with
		| [] -> true
		| h::t ->
			let x_prime = (dagger x h) in
				(match x_prime with
				| [] -> false
				| _ -> (iter_until_true x_prime (Star_imp x) t)))

  let smatch: string -> c -> bool =
    fun str code ->
		(match_string (c_to_c_imp code) (string_to_char_list str))
end
