module type SKI = sig
  type liquid =
    | S
    | K
    | I
    | V of string (* varible *)
    | M of liquid * liquid (* mix of two liquids *)
  val react: liquid -> liquid
  val pprint: liquid -> string
end

module SkiLiquid : SKI = struct
  exception ETODO

  type liquid =
    | S
    | K
    | I
    | V of string (* varible *)
    | M of liquid * liquid (* mix of two liquids *)

  let rec react: liquid -> liquid =
    fun l ->
      match l with
      | M (l_liq, r_liq) ->
        let l_liq_act = (react l_liq) in
        (match l_liq_act with
         | I -> (react r_liq)
         | M (deep_l_liq, deep_r_liq) ->
           (match deep_l_liq with
            | K -> (react deep_r_liq)
            | M (a, b) ->
              (match a with
               | S -> (react (M (M (b, r_liq), M (deep_r_liq, r_liq))))
               | _ -> M (l_liq_act, (react r_liq)))
            | _ -> M (l_liq_act, (react r_liq)))
         | _ -> M (l_liq_act, (react r_liq)))
      | _ -> l

  let pprint: liquid -> string =
    let rec printing e =
      match e with
      | S -> "S"
      | K -> "K"
      | I -> "I"
      | V x -> x
      | M (l, r) -> "(" ^ (printing l) ^ " " ^ (printing r) ^ ")" in
    fun l -> (printing l)
end
