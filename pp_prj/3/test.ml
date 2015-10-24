let temp = ref 0

let check_exp n = 
	temp := (!temp) + 1; !temp

let _ = print_int (check_exp 1)
let _ = print_int (check_exp 1)
let _ = print_int (check_exp 1)

let cnt_vars = ref 0
 print_int !cnt_vars ;

 print_endline "";

 let inc_cnt (n: int): int =
   cnt_vars := (!cnt_vars + 1);
     !cnt_vars

	 print_endline (string_of_int (inc_cnt 1));
