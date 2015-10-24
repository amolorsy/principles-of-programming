open CommonGrade
open Prj2

let _ =
  output
    (fun () ->
     let mat = Markov.add_column
                 [0.;1.;0.] 
                 (Markov.add_column
                    [1./.3.;1./.3.;1./.3.] 
                    (Markov.column [1./.2.;1./.2.;0.]))
     in
     let minit = Markov.column [1.;1.;1.] in
     let result = Markov.markov_limit mat minit in
     (abs_float (Markov.ij result 0 0 -. 1.) < 0.001
      && abs_float (Markov.ij result 0 1 -. 1.5) < 0.001
      && abs_float (Markov.ij result 0 2 -. 0.5) < 0.001))
