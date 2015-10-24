exception TODO

type crazy2 =
  | NIL
  | ZERO of crazy2
  | ONE of crazy2
  | MONE of crazy2

let rec power_num n =
    if n = 0 then 1 else 2 * (power_num (n - 1));;

let rec zpn n cnt =
    match n with
    | NIL -> 0
    | ZERO x -> 0 * (power_num cnt) + (zpn x (cnt + 1))
    | ONE x -> 1 * (power_num cnt) + (zpn x (cnt + 1)) 
    | MONE x -> -1 * (power_num cnt) + (zpn x (cnt + 1));;

let rec crazy2val (c: crazy2): int =
        (zpn c 0);;
