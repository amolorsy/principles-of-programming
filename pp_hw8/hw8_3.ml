module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module type ArgTy = 
sig
  type t
  val is_eq : t -> t -> bool
end

module QueueMake (Arg: ArgTy) 
  : Queue with type element = Arg.t =
struct
  type element = Arg.t
  type queue = element list * element list

  exception EMPTY_Q

  let is_exist p q =
    let l = fst q
    and r = snd q in
    (List.exists p l) || (List.exists p r)

  let emptyq = ([], [])
  let enq = 
    fun (q, e) ->
      if (is_exist (fun x -> Arg.is_eq e x) q) = true then q
      else (e::fst q, snd q)

  let deq =
    fun q ->
      let rev_snd = List.rev (snd q) in
      match rev_snd with
      | [] ->
        let rev_fst = List.rev (fst q) in
        (match rev_fst with
         | [] -> raise EMPTY_Q
         | h::t -> (h, (List.rev t, snd q)))
      | h::t -> (h, (fst q, List.rev t))
end
