exception TODO

type formula = 
  | TRUE
  | FALSE
  | NOT of formula
  | ANDALSO of formula * formula
  | ORELSE of formula * formula
  | IMPLY of formula * formula
  | LESS of expr * expr
and expr =
  | NUM of int
  | PLUS of expr * expr
  | MINUS of expr * expr

let rec get_value e =
  match e with
  | NUM n -> n
  | PLUS (a, b) -> (get_value a) + (get_value b)
  | MINUS (a, b) -> (get_value a) - (get_value b);;

let rec eval (f: formula): bool =
  match f with
  | TRUE -> true
  | FALSE -> false
  | NOT fm -> not (eval fm)
  | ANDALSO (a, b) -> (eval a) && (eval b)
  | ORELSE (a, b) -> (eval a) || (eval b)
  | IMPLY (a, b) ->
    if ((eval a) = true && (eval b) = false)
    then false else true
  | LESS (a, b) ->
    if ((get_value a) < (get_value b))
    then true else false;;
