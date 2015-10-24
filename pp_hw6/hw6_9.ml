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

let get_carry (l, r, c) =
  let sum_num = l + r + c in
  if sum_num > 1 then 1
  else if sum_num < -1 then -1
  else 0;;

let crazy2_convert (a, b, c) (x: crazy2): crazy2 =
  let sum_num = a + b + c in
  if sum_num = 3 then (ONE x)
  else if sum_num = 2 then (ZERO x)
  else if sum_num = 1 then (ONE x)
  else if sum_num = 0 then (ZERO x)
  else if sum_num = -1 then (MONE x)
  else if sum_num = -2 then (ZERO x)
  else (MONE x);;

let rec crazy2add_carry c p =
  match p with
  | (NIL, NIL) -> if c = 0 then NIL else if c = 1 then (ONE NIL) else (MONE NIL) 
  | (ZERO x, NIL) -> (crazy2_convert (c, 0, 0) (crazy2add_carry (get_carry (0, 0, c)) (x, NIL)))
  | (ONE x, NIL) -> (crazy2_convert (c, 1, 0) (crazy2add_carry (get_carry (1, 0, c)) (x, NIL)))
  | (MONE x, NIL) -> (crazy2_convert (c, -1, 0) (crazy2add_carry (get_carry (-1, 0, c)) (x, NIL)))
  | (NIL, ZERO x) -> (crazy2_convert (c, 0, 0) (crazy2add_carry (get_carry (0, 0, c)) (NIL, x)))
  | (NIL, ONE x) -> (crazy2_convert (c, 0, 1) (crazy2add_carry (get_carry (0, 1, c)) (NIL, x)))
  | (NIL, MONE x) -> (crazy2_convert (c, 0, -1) (crazy2add_carry (get_carry (0, -1, c)) (NIL, x)))
  | (ZERO x, ZERO y) -> (crazy2_convert (c, 0, 0) (crazy2add_carry (get_carry (0, 0, c)) (x, y)))
  | (ZERO x, ONE y) -> (crazy2_convert (c, 0, 1) (crazy2add_carry (get_carry (0, 1, c)) (x, y)))
  | (ZERO x, MONE y) -> (crazy2_convert (c, 0, -1) (crazy2add_carry (get_carry (0, -1, c)) (x, y)))
  | (ONE x, ZERO y) -> (crazy2_convert (c, 1, 0) (crazy2add_carry (get_carry (1, 0, c)) (x, y)))
  | (ONE x, ONE y) -> (crazy2_convert (c, 1, 1) (crazy2add_carry (get_carry (1, 1, c)) (x, y)))
  | (ONE x, MONE y) -> (crazy2_convert (c, 1, -1) (crazy2add_carry (get_carry (1, -1, c)) (x, y)))
  | (MONE x, ZERO y) -> (crazy2_convert (c, -1, 0) (crazy2add_carry (get_carry (-1, 0, c)) (x, y)))
  | (MONE x, ONE y) -> (crazy2_convert (c, -1, 1) (crazy2add_carry (get_carry (-1, 1, c)) (x, y)))
  | (MONE x, MONE y) -> (crazy2_convert (c, -1, -1) (crazy2add_carry (get_carry (-1, -1, c)) (x, y)));;

let rec crazy2add (a: crazy2) (b: crazy2): crazy2 =
  (crazy2add_carry 0 (a, b));;
