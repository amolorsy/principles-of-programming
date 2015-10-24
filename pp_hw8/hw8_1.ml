module type Queue = 
sig
  type element
  type queue
  exception EMPTY_Q
  val emptyq: queue
  val enq: queue * element -> queue
  val deq: queue -> element * queue
end

module StringQ : Queue with type element = string = 
struct 
  type element = string
  type queue = element list * element list

  exception EMPTY_Q

  let emptyq = ([], [])
  let enq =
    fun (q, e) -> ((e::fst q), snd q)
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

module StringQQ : Queue with type element = StringQ.queue = 
struct
  type element = StringQ.queue
  type queue = element list * element list

  exception EMPTY_Q

  let emptyq = ([], [])
  let enq =
    fun (q, e) -> ((e::fst q), snd q)
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
