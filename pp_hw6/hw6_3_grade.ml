open CommonGrade
open Hw6_3

let _ = output (fun () ->
    (drop (NODE (NODE (NODE (LEAF Korea, LEAF France), LEAF Brazil), LEAF England)) England) =
    "((Korea France) Brazil)");;

let c = "";; 

let a1 = (LEAF Korea);; 
let a2 = (LEAF France);; 
let a3 = (LEAF Usa);; 
let a4 = (LEAF Brazil);; 
let a5 = (LEAF Japan);; 
let a6 = (LEAF Nigeria);; 
let a7 = (LEAF Cameroon);; 
let a8 = (LEAF Poland);; 
let a9 = (LEAF Portugal);; 
let a10 = (LEAF Italy);; 
let a11 = (LEAF Germany);; 
let a12 = (LEAF Norway);; 
let a13 = (LEAF Sweden);; 
let a14 = (LEAF England);; 
let a15 = (LEAF Argentina);; 

let a16 = (NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil));; 
let a17 = (NODE(LEAF Norway, NODE(NODE(LEAF Cameroon, LEAF Poland), LEAF Sweden))) ;; 
let a18 = (NODE(NODE(LEAF Norway, NODE(LEAF Cameroon, LEAF Poland)), LEAF Sweden)) ;;

output (fun () -> drop a1 Korea = c);; 
output (fun () -> drop a2 France = c);; 
output (fun () -> drop a3 Usa = c);; 
output (fun () -> drop a4 Brazil = c);; 
output (fun () -> drop a5 Japan = c);; 
output (fun () -> drop a6 Nigeria = c);; 
output (fun () -> drop a7 Cameroon = c);; 
output (fun () -> drop a8 Poland = c);; 
output (fun () -> drop a9 Portugal = c);; 
output (fun () -> drop a10 Italy = c);; 
output (fun () -> drop a11 Germany = c);; 
output (fun () -> drop a12 Norway = c);; 
output (fun () -> drop a13 Sweden = c);; 
output (fun () -> drop a14 England = c);; 
output (fun () -> drop a15 Argentina = c);; 
output (fun () -> drop a2 Cameroon = "France");; 

output (fun () -> drop a16 Japan = "((Korea Portugal) Brazil)");; 
output (fun () -> drop a16 Korea = "(Portugal Brazil)");; 
output (fun () -> drop a16 Brazil = "(Korea Portugal)");; 
output (fun () -> drop a16 Portugal = "(Korea Brazil)");; 
output (fun () -> drop a17 Japan = "(Norway ((Cameroon Poland) Sweden))");; 
output (fun () -> drop a17 Norway = "((Cameroon Poland) Sweden)");; 
output (fun () -> drop a17 Cameroon = "(Norway (Poland Sweden))");; 
output (fun () -> drop a17 Poland = "(Norway (Cameroon Sweden))");; 
output (fun () -> drop a17 Sweden = "(Norway (Cameroon Poland))");; 

output (fun () -> drop a18 Japan = "((Norway (Cameroon Poland)) Sweden)");; 
output (fun () -> drop a18 Norway = "((Cameroon Poland) Sweden)");; 
output (fun () -> drop a18 Cameroon = "((Norway Poland) Sweden)");; 
output (fun () -> drop a18 Poland = "((Norway Cameroon) Sweden)");; 
output (fun () -> drop a18 Sweden = "(Norway (Cameroon Poland))");;
