open CommonGrade
open Hw7_2

let _ = output (fun () -> 
  "x" = 
    (SkiLiquid.pprint 
       (SkiLiquid.react 
          (SkiLiquid.M 
             (SkiLiquid.M (SkiLiquid.M (SkiLiquid.S,SkiLiquid.K),
                           SkiLiquid.I),
              SkiLiquid.V "x"))))
)

let _ = output (fun () -> 
  "x" = 
    (SkiLiquid.pprint
       (SkiLiquid.react 
          (SkiLiquid.M 
             (SkiLiquid.M (SkiLiquid.K,(SkiLiquid.V "x")),
              SkiLiquid.M (SkiLiquid.I,(SkiLiquid.V "x"))))))
)

let _ = output (fun () ->
  "(((x y) z) w)" =
    (SkiLiquid.pprint
       (SkiLiquid.M 
          (SkiLiquid.M 
             (SkiLiquid.M (SkiLiquid.V "x",SkiLiquid.V "y"),
              SkiLiquid.V "z"),
           SkiLiquid.V "w")))
)

let t1 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.K, SkiLiquid.V "3"), SkiLiquid.V "2") 
let t2 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.S, SkiLiquid.V "3"), SkiLiquid.V "2"), SkiLiquid.V "1") 
let t3 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.K, SkiLiquid.I), SkiLiquid.V "2"), SkiLiquid.V "3") 
let t4 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.V "H", SkiLiquid.V "e"), SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.K, SkiLiquid.M (SkiLiquid.I, SkiLiquid.S)), SkiLiquid.V "x"), SkiLiquid.M (SkiLiquid.I, SkiLiquid.I)), SkiLiquid.M (SkiLiquid.M (SkiLiquid.K, SkiLiquid.I), SkiLiquid.V "x")), SkiLiquid.V "l")), SkiLiquid.M (SkiLiquid.M (SkiLiquid.V "o", SkiLiquid.M (SkiLiquid.M (SkiLiquid.K, SkiLiquid.M (SkiLiquid.V ",", SkiLiquid.V "w")), SkiLiquid.V "x")), SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.I, SkiLiquid.V "o"), SkiLiquid.M (SkiLiquid.I, SkiLiquid.V "r")), SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.S, SkiLiquid.M (SkiLiquid.K, SkiLiquid.M (SkiLiquid.S, SkiLiquid.K))), SkiLiquid.S), SkiLiquid.M (SkiLiquid.M (SkiLiquid.K, SkiLiquid.M (SkiLiquid.M (SkiLiquid.K, SkiLiquid.S), SkiLiquid.V "3")), SkiLiquid.V "5")), SkiLiquid.M (SkiLiquid.V "l", SkiLiquid.V "d"))))) 

let _ = output (fun () -> "3" = (SkiLiquid.pprint (SkiLiquid.react t1))) 
let _ = output (fun () -> "((3 1) (2 1))" = (SkiLiquid.pprint (SkiLiquid.react t2))) 
let _ = output (fun () -> "3" = (SkiLiquid.pprint (SkiLiquid.react t3))) 
let _ = output (fun () -> "(((H e) (l l)) ((o (, w)) ((o r) (l d))))" = (SkiLiquid.pprint (SkiLiquid.react t4))) 

let t5 = SkiLiquid.M (SkiLiquid.I, SkiLiquid.V "x") 
let t6 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.K, SkiLiquid.V "x"), SkiLiquid.V "y") 
let t7 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.S, SkiLiquid.V "x"), SkiLiquid.V "y"), SkiLiquid.V "z") 
let t8 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.S, SkiLiquid.K), SkiLiquid.S), SkiLiquid.K) 
let t9 = SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.M (SkiLiquid.S, SkiLiquid.M (SkiLiquid.K, SkiLiquid.M (SkiLiquid.S, SkiLiquid.I))), SkiLiquid.K), SkiLiquid.V "x"), SkiLiquid.V "y") 

let _ = output (fun () -> "x" = (SkiLiquid.pprint (SkiLiquid.react t5))) 
let _ = output (fun () -> "x" = (SkiLiquid.pprint (SkiLiquid.react t6))) 
let _ = output (fun () -> "((x z) (y z))" = (SkiLiquid.pprint (SkiLiquid.react t7))) 
let _ = output (fun () -> "K" = (SkiLiquid.pprint (SkiLiquid.react t8))) 
let _ = output (fun () -> "(y x)" = (SkiLiquid.pprint (SkiLiquid.react t9)))
