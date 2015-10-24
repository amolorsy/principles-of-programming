exception TODO

type ae =
  | CONST of int
  | VAR of string
  | POWER of string * int
  | TIMES of ae list
  | SUM of ae list

let ae4=TIMES[SUM[TIMES[CONST 2;POWER("x",2)];VAR "x";VAR "y"];SUM[TIMES[CONST 5;POWER("x",2);VAR "y"];POWER("y",2)]];;
let a = SUM([TIMES([VAR "x";VAR "y"]);POWER("x", 3);CONST 7;TIMES([POWER("x", 3);POWER("y", 2)])]);;
let b = TIMES([SUM([VAR "x"; VAR "y"]);SUM([VAR "y"; VAR "z"]);SUM([VAR "z"; VAR "x"])]);;
let c = SUM([TIMES([VAR "x"; VAR "y"]);TIMES([VAR "y"; VAR "z"]);TIMES([VAR "z"; VAR "x"])]);;

let rec is_contain x str =
    match x with
	| CONST n -> false
    | VAR n -> if str = n then true else false
    | POWER (a, b) -> if str = a then true else false
	| TIMES lst -> if (List.length (List.filter (fun e -> is_contain e str) lst)) > 0 then true else false  
    | SUM lst -> if (List.length (List.filter (fun e -> is_contain e str) lst)) > 0 then true else false;;

let rec diff (e: ae) (x: string): ae =
    match e with
    | CONST n -> CONST 0
    | VAR str -> if str = x then CONST 1 else CONST 0
    | POWER (str, n) -> if str = x
        then TIMES [CONST n; POWER (str, (n - 1))]
        else CONST 0
	| TIMES lst ->
		let mode = List.length (List.filter (fun em -> is_contain em x) lst) in
			if mode = 0 then CONST 0
			else if mode = 1 then TIMES (List.map (fun item -> if (is_contain item x) then (diff item x) else item) lst)
			else
				let able_diff = (List.filter (fun able -> is_contain able x) lst)
				and not_able_diff = (List.filter (fun not_able -> not (is_contain not_able x)) lst) in
					let tl_able_diff = if (List.length (List.tl able_diff)) = 1
									   then List.hd (List.tl able_diff)
									   else TIMES (List.tl able_diff) in
						let not_diff_part = not_able_diff
						and diff_part = (SUM [TIMES (List.concat [[diff (List.hd able_diff) x]; List.tl able_diff]);
										TIMES [List.hd able_diff; diff tl_able_diff x]]) in
							if List.length not_diff_part = 0 then diff_part
							else TIMES (List.concat [not_diff_part; [diff_part]])
    | SUM lst -> SUM (List.map (fun em -> (diff em x)) lst);;
