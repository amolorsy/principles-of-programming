(* TA's comment:
 * 1. Implement step_tm as well as run_tm.
 *
 * 2. Functions print_tape and print_tm has different type from the direction.
 * Follow the types in this skeleton code.
 *)

module type TM = sig
  type symbol = string
  type move = Right | Left | Stay
  type todo = Erase | Write of symbol
  type state = string
  type rule = state * symbol * todo * move * state
  type ruletable = rule list
  type tape
  type tm
  (* tape part *)
  val init_tape: symbol list -> tape
  val read_tape: tape -> symbol
  val write_tape: tape -> symbol -> tape
  val move_tape_left: tape -> tape
  val move_tape_right: tape -> tape
  val print_tape: tape -> int -> string (* instead of tape -> unit *)
  (* rule table part *)
  val match_rule: state -> symbol -> ruletable -> (todo * move * state) option
  (* instead of state -> symbol -> ruletable -> todo * move * state *)
  (* main *)
  val make_tm: symbol list -> state -> ruletable -> tm
  (* instead of symbol list -> state list -> state -> ruletable -> tm *)
  val step_tm: tm -> tm option (* You should implement this. *)
  val run_tm: tm -> tm
  val print_tm: tm -> int -> string (* instead of tm -> int -> unit *)
end

module TuringMachine : TM = struct
  exception ETODO

  type symbol = string
  type move = Right | Left | Stay
  type todo = Erase | Write of symbol
  type state = string
  type rule = state * symbol * todo * move * state
  type ruletable = rule list

  type tape = int * (symbol list) (* should be replaced *)
  type tm = tape * state * ruletable (* should be replaced *)


  (* tape part *)
  let init_tape: symbol list -> tape =
    fun symbols -> (0, symbols)
          
  let read_tape: tape -> symbol =
    fun tape ->
		let syms = (snd tape) in
			let head = (fst tape) in
				match syms with
				| [] -> "-"
				| _ -> List.nth syms head

  let write_tape: tape -> symbol -> tape =
  	let rec writing lst n sym =
		match lst with
		| [] -> [sym]
		| h::t ->
			if n = 0 then sym::t
			else h::(writing t (n - 1) sym) in
    fun tape s ->
		let head = (fst tape) in
			(head, (writing (snd tape) head s))

  let move_tape_left: tape -> tape =
    fun tape -> 
		let next_head = (fst tape) + 1
		and syms = (snd tape) in
			if next_head >= (List.length syms)
			then (next_head, (List.append syms ["-"]))
			else (next_head, syms)

  let move_tape_right: tape -> tape =
    fun tape ->
    	let next_head = (fst tape) - 1
		and syms = (snd tape) in
			if next_head < 0
			then (0, (List.append ["-"] syms))
			else (next_head, syms)

  let rec move_tape_right_iter =
  	fun tape n ->
		if n = 0 then tape
		else (move_tape_right_iter (move_tape_right tape) (n - 1))
	
  let print_tape: tape -> int -> string =
  (* instead of tape -> unit *)
  	let rec printing =
		fun tp n ->
			let sym = (read_tape tp) in
				if n = 1 then sym
				else sym ^ "." ^ (printing (move_tape_left tp) (n - 1)) in
    fun tape size ->
		let mid = (move_tape_right_iter tape size) in
			(printing mid (2 * size + 1))
	

  (* rule table part *)	 
  let match_rule: state -> symbol -> ruletable -> (todo * move * state) option =
  	let rec mapping cur_st cur_sym rule_tbl = 
  		match rule_tbl with
  		| [] -> None
  		| h::t ->
			(match h with
			| (a, b, c, d, e) ->
				if a = cur_st && b = cur_sym
	  			then Some (c, d, e)
	  			else (mapping cur_st cur_sym t)) in
	fun st sym rules -> (mapping st sym rules)


  (* main *)
  let make_tm: symbol list -> state -> ruletable -> tm =
    (* instead of symbol list -> state list -> state -> ruletable -> tm *)
    fun symbols initial_state rules -> (init_tape symbols, initial_state, rules)

  let step_tm: tm -> tm option =
    fun tm ->
		match tm with
		| (tp, cur_st, tbl) ->
			let rd_sym = (read_tape tp) in
				let next_rule = (match_rule cur_st rd_sym tbl) in
					(match next_rule with
					| None -> None
					| Some (a, b, c) ->
						(match (a, b) with
						| (Erase, Left) -> Some ((move_tape_right (write_tape tp "-")), c, tbl)
						| (Erase, Right) -> Some ((move_tape_left (write_tape tp "-")), c, tbl)
						| (Erase, Stay) -> Some ((write_tape tp "-"), c, tbl)
						| (Write x, Left) -> Some ((move_tape_right (write_tape tp x)), c, tbl)
						| (Write x, Right) -> Some ((move_tape_left (write_tape tp x)), c, tbl)
						| (Write x, Stay) -> Some ((write_tape tp x), c, tbl)))

  let rec run_tm: tm -> tm =
    fun tm ->
		let one_step = (step_tm tm) in
			match one_step with
			| None -> tm
			| Some turing -> if turing = tm then tm else (run_tm turing)

  let print_tm: tm -> int -> string =
  (* instead of tm -> int -> unit *)
    fun tm size ->
		match tm with
		| (tp, st, tbl) -> (print_tape tp size)
end
