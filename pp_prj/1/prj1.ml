exception TODO

(* Project #1
 * You can assume that no three points are collinear.
*)
module type DUSTSTORM =
sig
  type robot = string (* robot’s name *)
  type shelter = int (* shelter’s id number *)
  type location = int * int (* 화성에서의 위도와 경도 (coordinate) *)
  type robot_locs = (robot * location) list
  type shelter_locs = (shelter * location) list
  val shelterAssign: robot_locs -> shelter_locs -> (robot * shelter) list
end

module Duststorm =
struct
  type robot = string (* robot’s name *)
  type shelter = int (* shelta’s id number *)
  type location = int * int (* 화성에서의 위도와 경도 (coordinate) *)
  type robot_locs = (robot * location) list
  type shelter_locs = (shelter * location) list

  let is_cross line1 line2 =
    let (p1, p2) = (fst line1, snd line1) in
    let (p3, p4) = (fst line2, snd line2) in
    let direction point_a point_b point_c =
      let x_a_b = (fst point_b) - (fst point_a)
      and y_a_b = (snd point_b) - (snd point_a)
      and x_a_c = (fst point_c) - (fst point_a)
      and y_a_c = (snd point_c) - (snd point_a) in
      if x_a_b * y_a_c < y_a_b * x_a_c then 1
      else if x_a_b * y_a_c > y_a_b * x_a_c then -1
      else if x_a_b = 0 && y_a_b = 0 then 0
      else if x_a_b * x_a_c < 0 || y_a_b * y_a_c < 0 then -1
      else if x_a_b * x_a_b + y_a_b * y_a_b >= x_a_c * x_a_c + y_a_c * y_a_c then 0
      else 1 in
    if ((direction p1 p2 p3) * (direction p1 p2 p4) <= 0) &&
       ((direction p3 p4 p1) * (direction p3 p4 p2) <= 0)
    then true else false

  let rec check_intersect lst r s record =
    match lst with
    | [] ->
      (match record with
       | [] -> [(r, s)]
       | hd::tl -> [(r, s)]@(check_intersect tl (fst hd) (snd hd) []))
    | h::t ->
      if (is_cross (snd (fst h), snd (snd h)) (snd r, snd s)) = true
      then (check_intersect t r (snd h) (record@[(fst h, s)]))
      else (check_intersect t r s (record@[(fst h, snd h)]))

  let print_result lst =
    List.map (fun ((id_s, loc_s), (id_e, loc_e)) -> (id_s, id_e)) lst

  let shelterAssign robot_locs shelter_locs =
    let rec iter_assign r_locs s_locs match_lst =
      match (r_locs, s_locs) with
      | ([r], [s]) ->
        (match match_lst with
         | [] -> (print_result [(r, s)])
         | _ -> (print_result (check_intersect match_lst r s [])))
      | ([r1; r2], [s1; s2]) ->
        (match match_lst with
         | [] ->
           if (is_cross (snd r1, snd s1) (snd r2, snd s2)) = true
           then (print_result [(r1, s2); (r2, s1)])
           else (print_result [(r1, s1); (r2, s2)])
         | _ ->
           let mid = (check_intersect match_lst r1 s1 []) in
           (print_result (check_intersect mid r2 s2 [])))
      | (r_f::r_s::r_t, s_f::s_s::s_t) ->
        (match match_lst with
         | [] ->
           if (is_cross (snd r_f, snd s_f) (snd r_s, snd s_s)) = true
           then (iter_assign r_t s_t [(r_f, s_s); (r_s, s_f)])
           else (iter_assign r_t s_t [(r_f, s_f); (r_s, s_s)])
         | _ ->
           let mid_p = (check_intersect match_lst r_f s_f []) in
           let mid_n = (check_intersect mid_p r_s s_s []) in
           (iter_assign r_t s_t mid_n))
      | (_, _) -> raise TODO in
    (iter_assign robot_locs shelter_locs [])
end

(* Project 1 Comment *)

(* 1. Make a list of pairs of robot and shelter.
   The example is [(r1, s1); (r2, s2); ...] *)
(* 2. I will get the output, which is a list of pairs of robot and shelter, and each pairs must not collide. And I solve the problem using recursion. *)
(* 3-1 When a length of the input list is 1, output is same as the input list. *)
(* 3-2 When a length of the input list is 2, output is a list that length of list is 2 and each pairs don't collide. *)
(* 3-3 When a length of the input list is bigger than 2, check firstly two pairs(list index 0, list index 1) of the input list whether to collide or not. If two pairs collide, solve collision. And about next two pairs(list index 1, list index 2), perform procedure 3-3 recursively. By recursion, I will finally come to check two pairs of last index in the input list, which is same as calling the base case(procedure 3-2) and the recursion terminates. *)
