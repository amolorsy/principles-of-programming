module type TRANS = 
sig
  type exp = 
  | Num of int
  | Add of exp * exp
  | Minus of exp
  | Read
  | If of exp * exp * exp
  | Repeat of exp * exp
  type var = string
  type tag = string
  type cmd = 
  | HasNum of var * int
  | HasVar of var * var
  | HasSum of var * var * var
  | HasSub of var * var * var
  | HasRead of var
  | Say of var
  | Goto of tag * var
  | Tag of tag * cmd
  | Seq of cmd * cmd
  val transform: exp -> cmd
  val check_exp: exp -> bool
  val check_cmd: cmd -> bool
end
