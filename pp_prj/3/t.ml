exception Todo

type exp = 
  | Num of int
  | Add of exp * exp
  | Minus of exp
  | Read
  | If of exp * exp * exp
  | Repeat of exp * exp
type var = string
type tag = string
type cmd = 
  | HasNum of var * int
  | HasVar of var * var
  | HasSum of var * var * var
  | HasSub of var * var * var
  | HasRead of var
  | Say of var
  | Goto of tag * var
  | Tag of tag * cmd
  | Seq of cmd * cmd

let rec is_contain_read (e:exp) : bool =
  match e with
  | Num n -> false
  | Add (l, r) -> (is_contain_read l) || (is_contain_read r)
  | Minus n -> (is_contain_read n)
  | Read -> true
  | If (x, l, r) -> (is_contain_read x) || (is_contain_read l) || (is_contain_read r)
  | Repeat (n, x) -> (is_contain_read n) || (is_contain_read x)

let rec get_exp_val (input:exp) : int =
  match input with
  | Num n -> n
  | Add (l, r) -> (get_exp_val l) + (get_exp_val r)
  | Minus n -> -1 * (get_exp_val n)
  | Read -> -100
  | If (x, l, r) -> if (get_exp_val x) != 0 then (get_exp_val l) else (get_exp_val r)
  | Repeat (n, x) -> (get_exp_val n) * (get_exp_val x)

let get_ret_var (c:cmd) : var =
  match c with
  | Seq (content, Say ret_var) -> ret_var
  | _ -> raise Todo
let get_content (c:cmd) : cmd =
  match c with
  | Seq (content, Say ret_var) -> content
  | _ -> raise Todo

let rec rec_seq (c:cmd) (n:int) : cmd =
  if n = 1 then c else Seq (c, (rec_seq c (n - 1)))

let transform (e:exp) : cmd =
  let var_count = ref 0 in
  let tag_count = ref 0 in
  let ret_var = ref "" in
  let rec conversion (p:exp) : cmd =
    let content =
      (match p with
       | Num n ->
         let now_var = "v" ^ (string_of_int !var_count) in
         var_count := !var_count + 1;
         ret_var := now_var;
         HasNum (now_var, n)
       | Add (l, r) ->
         let (cv_l, cv_r) = (conversion l, conversion r) in
         let now_var = "v" ^ (string_of_int !var_count) in
         var_count := !var_count + 1;
         ret_var := now_var;
         Seq (Seq (get_content cv_l, get_content cv_r), HasSum (now_var, get_ret_var cv_l, get_ret_var cv_r))
       | Minus n ->
         let (cv_l, cv_r) = (conversion (Num 0), conversion n) in
         let now_var = "v" ^ (string_of_int !var_count) in
         var_count := !var_count + 1;
         ret_var := now_var;
         Seq (Seq (get_content cv_l, get_content cv_r), HasSub (now_var, get_ret_var cv_l, get_ret_var cv_r))
       | Read ->
         let now_var = "v" ^ (string_of_int !var_count) in
         var_count := !var_count + 1;
         ret_var := now_var;
         HasRead now_var
       | If (x, l, r) ->
         let exp_val = (get_exp_val x) in
         let cmp_exp = (conversion x) in
         let true_exp = (conversion l) in
         let false_exp = (conversion r) in
         let now_tag = "t" ^ (string_of_int !tag_count) in
         let cmp_var = (get_ret_var cmp_exp) in 
         tag_count := !tag_count + 1;
         ret_var := (if exp_val != 0 then (get_ret_var true_exp) else (get_ret_var false_exp));
         Seq (get_content cmp_exp, (Seq (Goto (now_tag, cmp_var), (Seq (get_content false_exp, Tag (now_tag, get_content true_exp))))))
       | Repeat (n, x) ->
         (match (is_contain_read n) with
          | true -> (get_content (conversion x))
          | false ->
            (match (is_contain_read x) with
             | true ->
               (match n with
                | Num loop_n ->
                  if loop_n < 0 then (get_content (conversion x))
                  else if loop_n = 0 then
                    let now_var = "v" ^ (string_of_int !var_count) in
                    var_count := !var_count + 1;
                    ret_var := now_var;
                    HasNum (now_var, 0)
                  else 
                    let cv = (conversion x) in
                    let now_var = "v" ^ (string_of_int !var_count) in
                    var_count := !var_count + 1;
                    ret_var := now_var;
                    let read_var = (get_ret_var cv) in
                    Seq (HasNum (now_var, 0), rec_seq (Seq (get_content cv, HasSum (now_var, now_var, read_var))) loop_n)					| _ -> (get_content (conversion (Repeat (Num (get_exp_val n), x)))))
             | false ->
               (match n with
                | Num loop_n ->
                  if loop_n < 0 then (get_content (conversion x))
                  else if loop_n = 0 then
                    let now_var = "v" ^ (string_of_int !var_count) in
                    var_count := !var_count + 1;
                    ret_var := now_var;
                    HasNum (now_var, 0)
                  else
                    let cv = (conversion x) in
                    let operand = (get_ret_var cv) in
                    let now_var = "v" ^ (string_of_int !var_count) in
                    var_count := !var_count + 1;
                    ret_var := now_var;
                    Seq (get_content cv, Seq (HasNum (now_var, 0), rec_seq (HasSum (now_var, now_var, operand)) loop_n))
                | _ -> (get_content (conversion (Repeat (Num (get_exp_val n), x)))))))) in
    let return_var = !ret_var in
    Seq (content, (Say return_var)) in
  (conversion e)

let rec check_exp (e:exp) : bool =
  match e with
  | Num n -> true
  | Add (l, r) ->
    let fst_exp = (check_exp l) in
    if fst_exp = false then false else (check_exp r)
  | Minus n -> (check_exp n)
  | Read -> true
  | If (x, l, r) ->
    let exp_bool = (check_exp x) in
    if exp_bool = false then false
    else let snd_exp = (check_exp l) in
      if snd_exp = false then false
      else (check_exp r)
  | Repeat (n, x) ->
    if (check_exp n) = true && (get_exp_val n) < 0 then false
    else if (check_exp n) = false then false
    else (check_exp x)

let rec counting (lst:tag list) (obj:tag) (n:int) : int =
  match lst with
  | [] -> n
  | h::t -> if h = obj then (counting t obj (n + 1)) else (counting t obj n)

let is_tag_valid (l:tag list) (r:tag list) =
  (List.for_all (fun x -> (counting r x 0) = 1) l)

let check_cmd (c:cmd) : bool =
  let var_queue = ref [] in
  let tag_queue = ref [] in
  let goto_tag_queue = ref [] in
  let rec check_cmd_rec (command:cmd) : bool =
    match command with
    | HasNum (v, n) ->
      var_queue := v::(!var_queue); true
    | HasVar (v_l, v_r) ->
      if (List.exists (fun x -> x = v_r) !var_queue) = true
      then let _ = (var_queue := v_l::(!var_queue)) in true
      else false
    | HasSum (v, l, r) ->
      if (List.exists (fun x -> x = l) !var_queue) = true && (List.exists (fun y -> y = r) !var_queue) = true
      then let _ = (var_queue := v::(!var_queue)) in true
      else false
    | HasSub (v, l, r) ->
      if (List.exists (fun x -> x = l) !var_queue) = true && (List.exists (fun y -> y = r) !var_queue) = true
      then let _ = (var_queue := v::(!var_queue)) in true
      else false
    | HasRead v ->
      var_queue := v::(!var_queue); true
    | Say x ->
      let is_valid = (is_tag_valid !goto_tag_queue !tag_queue) in
      if is_valid = false then false
      else if (List.exists (fun e -> e = x) !var_queue) = true then true
      else false
    | Goto (t, v) ->
      if (List.exists (fun e -> e = v) !var_queue) = false
      then false
      else
        (if (List.exists (fun x -> x = t) !goto_tag_queue) = false
         then let _ = goto_tag_queue := t::(!goto_tag_queue) in true else true)
    | Tag (t, c_t) ->
      if (List.exists (fun e -> e = t) !tag_queue) = false
      then let _ = tag_queue := t::(!tag_queue) in (check_cmd_rec c_t)
      else false
    | Seq (c_l, c_r) ->
      let check_c_l = (check_cmd_rec c_l) in
      if check_c_l = false then false else (check_cmd_rec c_r) in
  (check_cmd_rec c)

type indent = int
let string_of_indent (idt:indent) : string = String.make idt ' '
let rec string_of_exp (idt:indent) (e:exp) : string =
  (string_of_indent idt) ^
  (match e with
   | Num i -> string_of_int i
   | Add (e1, e2) -> 
     "+\n" ^
     (string_of_exp (idt + 2) e1) ^ "\n" ^
     (string_of_exp (idt + 2) e2)
   | Minus e1 -> "-\n" ^ (string_of_exp (idt + 2) e1)
   | Read -> "read"
   | If (e1, e2, e3) -> 
     "if\n" ^
     (string_of_exp (idt + 2) e1) ^ "\n" ^
     (string_of_exp (idt + 2) e2) ^ "\n" ^
     (string_of_exp (idt + 2) e3)
   | Repeat (e1, e2) -> 
     "repeat\n" ^
     (string_of_exp (idt + 2) e1) ^ "\n" ^
     (string_of_exp (idt + 2) e2)
  )
let pprint_exp (e:exp) : unit = print_endline (string_of_exp 0 e)

let rec string_of_cmd (c:cmd) : string = 
  match c with
  | HasNum (x, i) -> x ^ " has " ^ (string_of_int i)
  | HasVar (x, y) -> x ^ " has " ^ y
  | HasSum (x, y, z) -> x ^ " has " ^ y ^ "+" ^ z
  | HasSub (x, y, z) -> x ^ " has " ^ y ^ "-" ^ z
  | HasRead x -> x ^ " has read"
  | Say x -> "say " ^ x
  | Goto (t, x) -> "goto " ^ t ^ " on " ^ x
  | Tag (t, c1) -> t ^ ": " ^ (string_of_cmd c1)
  | Seq (c1, c2) -> "(" ^ (string_of_cmd c1) ^ " ;\n" ^ (string_of_cmd c2) ^ ")"
let pprint_cmd (c:cmd) : unit = print_endline (string_of_cmd c);;
