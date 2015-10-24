exception TODO

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina

type tourna =
    LEAF of team
  | NODE of tourna * tourna

let string_of_team t =
  match t with
  | Korea -> "Korea"
  | France -> "France"
  | Usa -> "Usa"
  | Brazil -> "Brazil"
  | Japan -> "Japan"
  | Nigeria -> "Nigeria"
  | Cameroon -> "Cameroon"
  | Poland -> "Poland"
  | Portugal -> "Portugal"
  | Italy -> "Italy"
  | Germany -> "Germany"
  | Norway -> "Norway"
  | Sweden -> "Sweden"
  | England -> "England"
  | Argentina -> "Argentina";;

let rec parenize (t: tourna): string =
  match t with
  | LEAF tm -> string_of_team tm
  | NODE (lNation, rNation) ->
    "(" ^ (parenize lNation) ^ " " ^ (parenize rNation) ^ ")";;

let rec drop (t: tourna) (d: team): string =
  match t with
  | LEAF t -> if t = d then "" else parenize (LEAF t)
  | NODE (LEAF x, LEAF y) ->
    if x = d then parenize (LEAF y)
    else if y = d then parenize (LEAF x)
    else parenize (NODE (LEAF x, LEAF y))
  | NODE (LEAF x, NODE (l, r)) ->
    if x = d then parenize (NODE (l, r))
    else "(" ^ (parenize (LEAF x)) ^ " " ^ (drop (NODE (l, r)) d) ^ ")"
  | NODE (NODE (l, r), LEAF y) ->
    if y = d then parenize (NODE (l, r))
    else "(" ^ (drop (NODE (l, r)) d) ^ " " ^ (parenize (LEAF y)) ^ ")"
  | NODE (NODE (l, r), NODE (l2, r2)) -> "(" ^ (drop (NODE (l, r)) d) ^ " " ^ (drop (NODE (l2, r2)) d) ^ ")";;
