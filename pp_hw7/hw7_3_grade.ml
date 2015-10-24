open CommonGrade
open Hw7_3
let _ = output (fun () -> true = (Smatch.smatch "1011010011010101000110" (Smatch.Star (Smatch.Mult (Smatch.Star Smatch.Zero, Smatch.Star Smatch.One)))))

let _ = output (fun () -> 
  true = 
    (Smatch.smatch "11"
       (Smatch.Mult
          (Smatch.Mult (Smatch.One,Smatch.Star Smatch.Zero),
           Smatch.Opt Smatch.One)))
)

let _ = output (fun () -> 
  false = 
    (Smatch.smatch "11"
       (Smatch.Mult
          (Smatch.Star (Smatch.Mult (Smatch.One,Smatch.Zero)),
           Smatch.One)))
)

let _ = output (fun () ->
  false =
    (Smatch.smatch "1111110101000001010101010121010101011010101"
	  (Smatch.Star
	    (Smatch.Mult (Smatch.Star (Smatch.Zero), Smatch.Star (Smatch.One))))))
