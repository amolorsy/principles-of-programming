exception TODO

module type MARKOV =
sig
    type matrix
    val row: float list -> matrix
    val column: float list -> matrix
    val add_row: float list -> matrix -> matrix
    val add_column: float list -> matrix -> matrix
    val size: matrix -> int * int (* numbers of columns and rows *)
    val ij: matrix -> int -> int -> float
    (*
    Given a Markov matrix and an initial column,
    markov_limit returns the limit of the Markov chain.
    *)	

	val markov_limit: matrix -> matrix -> matrix
end

module Markov : MARKOV =
struct
    type matrix = (float list) list

    let row flist =
      (List.map (fun x -> [x]) flist)
    let column flist =
      [flist]
    let add_row flist mat =
      (List.map2 (fun l r -> r@[l]) flist mat)
    let add_column flist mat =
	  mat@[flist]
    let size mat = (* numbers of columns and rows *)
	  let c_ = List.hd mat in
      	(List.length mat, List.length c_)
    let ij mat i j = (* i = column index, j = row index *)
	  (List.nth (List.nth mat i) j)

	(* you may find this function useful in debugging. *)
	let matrix_printer mat =
  		let (cs, rs) = size mat in
  		for j = 0 to rs - 1 do
    		for i = 0 to cs - 1 do
      		print_string "\t";
      		(print_float (ij mat i j))
   	 		done;
    		print_newline ()
 	 	done;
		print_newline ()

	let rec make_idx_lst x y =
		if y = 1 then [(x, y)] else [(x, y)]@(make_idx_lst x (y - 1))

	let rec find_nth lst x n =
		match lst with
		| [] -> -1
		| h::t -> if h = x then n else (find_nth t x (n + 1)) 

	let matrix_add l_m r_m =
		List.map2 (fun l r -> (List.map2 (fun l_f r_f -> l_f +. r_f) l r)) l_m r_m

	let matrix_mul l_m r_m =
		let (l_c, l_r) = size l_m
		and (r_c, r_r) = size r_m in
		let result_mul = ref [] in
		for i = 0 to r_c - 1 do
			let f_lst = ref [] in
			for j = 0 to l_r - 1 do
				let sum = ref 0.0 in
				for k = 0 to r_r - 1 do
					sum := !sum +. ((ij l_m k j) *. (ij r_m i k))
				done;
				f_lst := !f_lst@[!sum] 
			done;
			result_mul := !result_mul@[!f_lst]
		done;
		!result_mul

	let check_error l_m r_m =
		let abs_float f =
			if f > 0.0 then f
			else if f = 0.0 then 0.0
			else (-1.0 *. f) in
		let bool_lst =
			List.map2 (fun l r ->
				if (abs_float (l -. r)) < 0.0000001 then true else false) (List.flatten l_m) (List.flatten r_m) in
		let rec check_bool lst =
			match lst with
			| [] -> true
			| h::t -> if h = true then (check_bool t) else false in
		(check_bool bool_lst)

    let markov_limit mat initial_mat =
		let trans_mat alpha =
			let identity_mat =
				let rec make_idx_mat i j =
					if i = 1 then [(make_idx_lst i j)]
					else [(make_idx_lst i j)]@(make_idx_mat (i - 1) j) in
				let idx_mat = (make_idx_mat (fst (size mat)) (snd (size mat))) in
					(List.map (fun i ->
						(List.map (fun (l_idx, r_idx) -> if l_idx = r_idx then 1.0 else 0.0) i)) idx_mat) in
			let alpha_mat = List.map (fun i -> List.map (fun j -> alpha *. j) i) mat
			and alpha_identity = List.map (fun m -> List.map (fun n -> (1.0 -. alpha) *. n) m) identity_mat in
				(matrix_add alpha_mat alpha_identity) in
		let rec limit_rec weight =
			let mid = (matrix_mul (trans_mat 0.5) weight) in
				if mid = weight then weight
				else if (check_error mid weight) = true then weight
				else (limit_rec mid) in
		(limit_rec initial_mat)
end
