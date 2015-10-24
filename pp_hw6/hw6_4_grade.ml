open CommonGrade
open Hw6_4

let _ = output (fun () ->
  (eval (IMPLY (ORELSE (FALSE, LESS (NUM 0, (PLUS (NUM (-1), NUM 0)))), FALSE))));;

let f p q = eval (ANDALSO (p, (ORELSE (q, (ANDALSO ((NOT p), (NOT q)))))));; 
let r_f p q = not (f p q);; 
output (fun () -> (f TRUE TRUE) = true);; 
output (fun () -> (f TRUE FALSE) = false);; 
output (fun () -> (f FALSE TRUE) = false);; 
output (fun () -> (f FALSE FALSE) = false);; 
output (fun () -> (r_f TRUE TRUE) = false);; 
output (fun () -> (r_f TRUE FALSE) = true);; 
output (fun () -> (r_f FALSE TRUE) = true);; 
output (fun () -> (r_f FALSE FALSE) = true);; 

let g1 p q r = eval (IMPLY ((ANDALSO ((IMPLY (p,(ORELSE (q,r)))),(NOT (ORELSE (q,r))))),(NOT p)));; 
output (fun () -> (g1 TRUE TRUE TRUE) = true);; 
output (fun () -> (g1 TRUE TRUE FALSE) = true);; 
output (fun () -> (g1 TRUE FALSE TRUE) = true);; 
output (fun () -> (g1 TRUE FALSE FALSE) = true);; 
output (fun () -> (g1 FALSE TRUE TRUE) = true);; 
output (fun () -> (g1 FALSE TRUE FALSE) = true);; 
output (fun () -> (g1 FALSE FALSE TRUE) = true);; 
output (fun () -> (g1 FALSE FALSE FALSE) = true);; 

let g2 p q r = eval (ANDALSO ((IMPLY ((ORELSE (p,q)),(NOT r))),(ORELSE ((NOT r),(ORELSE (q,p))))));; 
output (fun () -> (g2 TRUE TRUE TRUE) = false);; 
output (fun () -> (g2 TRUE TRUE FALSE) = true);; 
output (fun () -> (g2 TRUE FALSE TRUE) = false);; 
output (fun () -> (g2 TRUE FALSE FALSE) = true);; 
output (fun () -> (g2 FALSE TRUE TRUE) = false);; 
output (fun () -> (g2 FALSE TRUE FALSE) = true);; 
output (fun () -> (g2 FALSE FALSE TRUE) = false);; 
output (fun () -> (g2 FALSE FALSE FALSE) = true);; 

let h1 p q r = eval (ORELSE ((NOT p),(ANDALSO (q,r))));; 
output (fun () -> (h1 TRUE TRUE TRUE) = true);; 
output (fun () -> (h1 TRUE TRUE FALSE) = false);; 
output (fun () -> (h1 TRUE FALSE TRUE) = false);; 
output (fun () -> (h1 TRUE FALSE FALSE) = false);; 
output (fun () -> (h1 FALSE TRUE TRUE) = true);; 
output (fun () -> (h1 FALSE TRUE FALSE) = true);; 
output (fun () -> (h1 FALSE FALSE TRUE) = true);; 
output (fun () -> (h1 FALSE FALSE FALSE) = true);; 

output (fun () -> eval(LESS(PLUS(NUM 5, NUM 5), MINUS(NUM 10, NUM 7)))=false);;
output (fun () -> eval(LESS(PLUS(NUM 5, NUM 5), MINUS(NUM 20, NUM 7)))=true);; 
output (fun () -> true = eval TRUE);; 
output (fun () -> false = eval FALSE);; 
output (fun () -> false = eval (NOT TRUE));; 
output (fun () -> false = eval (ANDALSO (TRUE, FALSE)));; 
output (fun () -> true = eval (ORELSE (TRUE, FALSE)));; 
output (fun () -> false = eval (LESS (PLUS(NUM 3, NUM 4), MINUS(NUM 7, NUM 8 ))));;
