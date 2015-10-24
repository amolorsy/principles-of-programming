open CommonGrade
open Hw7_1

(* Tape *)
let _ = print_endline "Tape"

let tape1 = TuringMachine.init_tape ["a";"b"]
let tape2 = TuringMachine.move_tape_left tape1
let tape3 = TuringMachine.move_tape_left tape2
let tape4 = TuringMachine.move_tape_right tape3
let tape5 = TuringMachine.move_tape_right tape4
let tape6 = TuringMachine.move_tape_right tape5
let tape7 = TuringMachine.write_tape tape6 "c";;

TuringMachine.print_tape tape1 2;;
let _ = output (fun () -> "-.-.a.b.-" = (TuringMachine.print_tape tape1 2))
let _ = output (fun () -> "-.a.b.-.-" = (TuringMachine.print_tape tape2 2))
let _ = output (fun () -> "a.b.-.-.-" = (TuringMachine.print_tape tape3 2))
let _ = output (fun () -> "-.a.b.-.-" = (TuringMachine.print_tape tape4 2))
let _ = output (fun () -> "-.-.a.b.-" = (TuringMachine.print_tape tape5 2))
let _ = output (fun () -> "-.-.-.a.b" = (TuringMachine.print_tape tape6 2))
let _ = output (fun () -> "-.-.c.a.b" = (TuringMachine.print_tape tape7 2))

let _ = output (fun () -> "a" = (TuringMachine.read_tape tape1))
let _ = output (fun () -> "b" = (TuringMachine.read_tape tape2))
let _ = output (fun () -> "c" = (TuringMachine.read_tape tape7))

(* Rule table *)
let _ = print_endline "Rule table"

let table = 
  [("1","a",TuringMachine.Write "d",TuringMachine.Right,"2");
   ("2","b",TuringMachine.Write "e",TuringMachine.Right,"3");
   ("3","-",TuringMachine.Write "f",TuringMachine.Left,"4");
   ("4","e",TuringMachine.Write "g",TuringMachine.Left,"5");
   ("5","d",TuringMachine.Write "h",TuringMachine.Left,"6");
   ("6","-",TuringMachine.Write "i",TuringMachine.Stay,"7")]

let table2 =
  [("empty","a",TuringMachine.Write "a", TuringMachine.Right, "carrying");
   ("carrying","a",TuringMachine.Write "a", TuringMachine.Right, "carrying")]
let rec steps_tm n tm =
	let tmp = (TuringMachine.step_tm tm) in
  		if n = 0 then tm
  			else (steps_tm (n - 1)
				(match tmp with
				| Some x -> x))
  

let tm_1 = (TuringMachine.make_tm ["a";"a";"a";"a";"a";"a";"a";"a"] "empty" table2)
let tm_2 = (steps_tm 8 tm_1);;

print_string "Assistant's Case";;
print_string (TuringMachine.print_tm (TuringMachine.make_tm ["e";"m";"p";"t";"y"] "empty" table2) 7);;
let _ = output (fun () ->
 "-.-.-.a.a.a.a" = (TuringMachine.print_tm tm_1 3))
let _ = output (fun () ->
 "a.a.a.-.-.-.-" = (TuringMachine.print_tm tm_2 3))
  
let _ = output (fun () -> 
  Some (TuringMachine.Write "d",TuringMachine.Right,"2") = 
    TuringMachine.match_rule "1" "a" table
)
let _ = output (fun () -> 
  Some (TuringMachine.Write "e",TuringMachine.Right,"3") = 
    TuringMachine.match_rule "2" "b" table
)
let _ = output (fun () -> 
  Some (TuringMachine.Write "f",TuringMachine.Left,"4") = 
    TuringMachine.match_rule "3" "-" table
)
let _ = output (fun () -> 
  Some (TuringMachine.Write "g",TuringMachine.Left,"5") = 
    TuringMachine.match_rule "4" "e" table
)
let _ = output (fun () -> 
  Some (TuringMachine.Write "h",TuringMachine.Left,"6") = 
    TuringMachine.match_rule "5" "d" table
)
let _ = output (fun () -> 
  Some (TuringMachine.Write "i",TuringMachine.Stay,"7") = 
    TuringMachine.match_rule "6" "-" table
)

(* Turaing machine *)
let _ = print_endline "Turing machine"

let rec step_n : int -> TuringMachine.tm -> TuringMachine.tm option = 
  fun n tm ->
    if n > 0 
    then 
      (match TuringMachine.step_tm tm with
      | Some tm' -> step_n (n-1) tm'
      | None -> None)
    else Some tm

let tm1 = TuringMachine.make_tm ["a";"b"] "1" table
let tm2_opt = step_n 1 tm1 
let tm3_opt = step_n 2 tm1
let tm4_opt = step_n 3 tm1
let tm5_opt = step_n 4 tm1
let tm6_opt = step_n 5 tm1
let tm7_opt = step_n 6 tm1
let tm8_opt = step_n 7 tm1
let tm9 = TuringMachine.run_tm tm1

let _ = output (fun () -> "-.-.a.b.-" = TuringMachine.print_tm tm1 2)
let _ = output (fun () -> 
  (match tm2_opt with
  | Some tm2 -> "-.d.b.-.-" = TuringMachine.print_tm tm2 2
  | None -> false
  ))
let _ = output (fun () -> 
  (match tm3_opt with
  | Some tm3 -> "d.e.-.-.-" = TuringMachine.print_tm tm3 2
  | None -> false
  ))
let _ = output (fun () -> 
  (match tm4_opt with
  | Some tm4 -> "-.d.e.f.-" = TuringMachine.print_tm tm4 2
  | None -> false
  ))
let _ = output (fun () -> 
  (match tm5_opt with
  | Some tm5 -> "-.-.d.g.f" = TuringMachine.print_tm tm5 2
  | None -> false
  ))
let _ = output (fun () -> 
  (match tm6_opt with
  | Some tm6 -> "-.-.-.h.g" = TuringMachine.print_tm tm6 2
  | None -> false
  ))
let _ = output (fun () -> 
  (match tm7_opt with
  | Some tm7 -> "-.-.i.h.g" = TuringMachine.print_tm tm7 2
  | None -> false
  ))

(* If no rule is applicable, just do nothing. *)

let _ = output (fun () -> 
  (match tm8_opt with
  | Some _ -> false
  | None -> true
  ))

let _ = output (fun () -> "-.-.i.h.g" = TuringMachine.print_tm tm9 2)
