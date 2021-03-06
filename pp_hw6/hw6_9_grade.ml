open CommonGrade
open Hw6_9

let _ = Random.self_init() ;; 

let rec _testGenerator n = 
  let rNum = Random.int n in 
  if n = 1 then NIL 
  else begin 
    let rNum = Random.int 3 in 
    if rNum = 2 then ONE (_testGenerator (n-1)) 
    else if rNum = 1 then ZERO (_testGenerator (n-1)) 
    else MONE (_testGenerator (n-1)) 
  end ;; 

let _test n = 
  let a = (_testGenerator n) in 
  let b = (_testGenerator n) in 
  print_endline "---------------";
  print_int (crazy2val a);
  print_string " + ";
  print_int (crazy2val b);
  print_string " = ";
  print_int ((crazy2val a) + (crazy2val b));
  ((crazy2val (crazy2add a b)) = 
   (crazy2val a) + (crazy2val b)) ;;

let _testN n' n = 
  for a = 0 to n'-1 
  do (print_string (if (_test n) then "success: #" else "failure: #"); 
      print_int a; print_string "\n") done ;;

_testN 1000 1000;;
