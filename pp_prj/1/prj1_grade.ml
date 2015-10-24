open CommonGrade
open Prj1
open Duststorm

exception Ta_Out_of_loop

let isOnSegment ((xi:float),  (yi:float), (xj:float), (yj:float), (xk:float), (yk:float)) =
  (xi <= xk || xj <= xk) && (xk <= xi || xk <= xj) &&
  (yi <= yk || yj <= yk) && (yk <= yi || yk <= yj)

let computeDirection ((xi:float), (yi:float), (xj:float), (yj:float), (xk:float), (yk:float)) =
  let a = (xk -. xi) *. (yj -. yi) in
  let b = (xj -. xi) *. (yk -. yi) in
  if (a < b) then -1
  else if (a > b) then 1
  else 0

let doLineSegmentsIntersect ((x1:float), (y1:float), (x2:float), (y2:float), (x3:float), (y3:float), (x4:float), (y4:float)) =
  let d1 = computeDirection (x3, y3, x4, y4, x1, y1) in
  let d2 = computeDirection (x3, y3, x4, y4, x2, y2) in
  let d3 = computeDirection (x1, y1, x2, y2, x3, y3) in
  let d4 = computeDirection (x1, y1, x2, y2, x4, y4) in
  (((d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)) && ((d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0))) ||
  (d1 == 0 && isOnSegment(x3, y3, x4, y4, x1, y1)) ||
  (d2 == 0 && isOnSegment(x3, y3, x4, y4, x2, y2)) ||
  (d3 == 0 && isOnSegment(x1, y1, x2, y2, x3, y3)) ||
  (d4 == 0 && isOnSegment(x1, y1, x2, y2, x4, y4))

let isIntersect rloc1 rloc2 sloc1 sloc2 =
  doLineSegmentsIntersect (float_of_int (fst rloc1), float_of_int (snd rloc1), float_of_int (fst sloc1), float_of_int (snd sloc1), float_of_int (fst rloc2), float_of_int (snd rloc2), float_of_int (fst sloc2), float_of_int (snd sloc2))

let hasIntersectArray r_array s_array =
  let result = ref false in
  (try (
     for i = 0 to (Array.length r_array)-1 do
       for j = i+1 to (Array.length (r_array))-1 do
         (if isIntersect (snd (r_array).(i)) (snd (r_array).(j)) (snd (s_array).(i)) (snd (s_array).(j)) then
            (result := true;
             (raise Ta_Out_of_loop))
          else
            ())
       done
     done)
   with Ta_Out_of_loop -> ());
  !result

let hasIntersect rList sList rsList=
  let (rl,sl) = List.split rsList in
  let rec sortlst l lst=
    match l with
    | [] -> []
    | h::t -> (List.find (fun x -> (fst x) = h) lst)::(sortlst t lst) in
  let rList' = sortlst rl rList in
  let sList' = sortlst sl sList in
  hasIntersectArray (Array.of_list rList') (Array.of_list sList')

let (robots1, shelters1) =  ([("r1", (54, 92)); ("r2", (94, 25)); ("r3", (29, 22)); ("r4", (57, 54));
                              ("r5", (88, 44)); ("r6", (16, 32)); ("r7", (8, 8)); ("r8", (77, 46));
                              ("r9", (9, 6)); ("r10", (36, 84))],
                             [(1, (90, 64)); (2, (77, 33)); (3, (12, 81)); (4, (9, 96)); (5, (70, 72));
                              (6, (99, 68)); (7, (74, 39)); (8, (91, 48)); (9, (1, 43)); (10, (18, 72))])

let (robots2, shelters2) = ([("r1", (19, 65)); ("r2", (5, 10)); ("r3", (1, 1)); ("r4", (89, 27));
                             ("r5", (31, 91)); ("r6", (87, 35)); ("r7", (50, 76)); ("r8", (72, 71));
                             ("r9", (3, 49)); ("r10", (94, 86))],
                            [(1, (21, 82)); (2, (66, 38)); (3, (63, 44)); (4, (51, 6)); (5, (0, 93));
                             (6, (71, 78)); (7, (77, 11)); (8, (48, 41)); (9, (43, 91)); (10, (71, 48))])

let (robots3, shelters3) =  ([("r1", (1, 12)); ("r2", (65, 17)); ("r3", (88, 22)); ("r4", (35, 73));
                              ("r5", (47, 70)); ("r6", (21, 96)); ("r7", (0, 85)); ("r8", (22, 32));
                              ("r9", (31, 63)); ("r10", (78, 22)); ("r11", (4, 94)); ("r12", (19, 70));
                              ("r13", (77, 6)); ("r14", (20, 92)); ("r15", (89, 15)); ("r16", (90, 90));
                              ("r17", (90, 73)); ("r18", (59, 97)); ("r19", (37, 30)); ("r20", (95, 51))],
                             [(1, (64, 56)); (2, (83, 78)); (3, (64, 80)); (4, (27, 21)); (5, (61, 83));
                              (6, (46, 7)); (7, (10, 96)); (8, (18, 59)); (9, (8, 78)); (10, (37, 68));
                              (11, (62, 59)); (12, (42, 7)); (13, (75, 19)); (14, (98, 80));
                              (15, (83, 37)); (16, (18, 16)); (17, (100, 95)); (18, (56, 43));
                              (19, (74, 68)); (20, (89, 46))])

let (robots4, shelters4) =  ([("r1", (39, 42)); ("r2", (100, 99)); ("r3", (66, 40)); ("r4", (61, 9));
                              ("r5", (1, 38)); ("r6", (23, 22)); ("r7", (8, 33)); ("r8", (64, 79));
                              ("r9", (10, 26)); ("r10", (85, 20)); ("r11", (3, 72)); ("r12", (3, 66));
                              ("r13", (60, 31)); ("r14", (13, 69)); ("r15", (80, 88)); ("r16", (75, 71));
                              ("r17", (27, 1)); ("r18", (23, 89)); ("r19", (58, 43)); ("r20", (49, 33))],
                             [(1, (17, 39)); (2, (28, 1)); (3, (99, 15)); (4, (38, 0)); (5, (58, 76));
                              (6, (100, 90)); (7, (17, 4)); (8, (56, 40)); (9, (50, 82)); (10, (27, 96));
                              (11, (63, 28)); (12, (39, 18)); (13, (94, 28)); (14, (59, 29));
                              (15, (11, 66)); (16, (65, 12)); (17, (4, 27)); (18, (87, 12));
                              (19, (49, 85)); (20, (90, 41))])

let (robots5, shelters5) =  ([("r1", (89, 91)); ("r2", (96, 73)); ("r3", (49, 70)); ("r4", (38, 38));
                              ("r5", (3, 16)); ("r6", (34, 89)); ("r7", (60, 100)); ("r8", (87, 99));
                              ("r9", (96, 9)); ("r10", (59, 47))],
                             [(1, (73, 11)); (2, (2, 43)); (3, (56, 68)); (4, (84, 11)); (5, (78, 0));
                              (6, (31, 81)); (7, (57, 51)); (8, (2, 73)); (9, (73, 2)); (10, (98, 22))])

let (robots6, shelters6) = ([("r1", (85, 90)); ("r2", (32, 8)); ("r3", (76, 53)); ("r4", (75, 29));
                             ("r5", (64, 65)); ("r6", (39, 81)); ("r7", (34, 54)); ("r8", (72, 5));
                             ("r9", (14, 48)); ("r10", (26, 25)); ("r11", (8, 85)); ("r12", (71, 99));
                             ("r13", (19, 96)); ("r14", (52, 72)); ("r15", (35, 90)); ("r16", (5, 54));
                             ("r17", (17, 75)); ("r18", (100, 48)); ("r19", (74, 100));
                             ("r20", (42, 86))],
                            [(1, (6, 93)); (2, (71, 87)); (3, (61, 36)); (4, (93, 67)); (5, (12, 10));
                             (6, (52, 44)); (7, (87, 51)); (8, (68, 7)); (9, (80, 87)); (10, (42, 84));
                             (11, (83, 10)); (12, (39, 80)); (13, (11, 81)); (14, (53, 38));
                             (15, (91, 91)); (16, (23, 97)); (17, (95, 20)); (18, (20, 80));
                             (19, (65, 52)); (20, (89, 63))])

let (robots7, shelters7) = ([("r1", (91, 97)); ("r2", (90, 25)); ("r3", (30, 62)); ("r4", (30, 64));
                             ("r5", (2, 75)); ("r6", (8, 67)); ("r7", (88, 40)); ("r8", (73, 8));
                             ("r9", (1, 31)); ("r10", (59, 16)); ("r11", (47, 66)); ("r12", (70, 100));
                             ("r13", (91, 1)); ("r14", (42, 53)); ("r15", (24, 55)); ("r16", (58, 24));
                             ("r17", (39, 4)); ("r18", (87, 73)); ("r19", (41, 18)); ("r20", (87, 86))],
                            [(1, (69, 54)); (2, (21, 19)); (3, (18, 47)); (4, (96, 22)); (5, (52, 87));
                             (6, (31, 19)); (7, (14, 29)); (8, (81, 12)); (9, (61, 81));
                             (10, (100, 14)); (11, (71, 37)); (12, (62, 71)); (13, (39, 24));
                             (14, (41, 95)); (15, (94, 11)); (16, (85, 42)); (17, (35, 53));
                             (18, (96, 63)); (19, (4, 65)); (20, (71, 48))])

let (robots8, shelters8) = ([("r1", (1, 12)); ("r2", (65, 17)); ("r3", (88, 22)); ("r4", (35, 73));
                             ("r5", (47, 70)); ("r6", (21, 96)); ("r7", (0, 85)); ("r8", (22, 32));
                             ("r9", (31, 63)); ("r10", (78, 22)); ("r11", (4, 94)); ("r12", (19, 70));
                             ("r13", (77, 6)); ("r14", (20, 92)); ("r15", (89, 15)); ("r16", (90, 90));
                             ("r17", (90, 73)); ("r18", (59, 97)); ("r19", (37, 30)); ("r20", (95, 51))],
                            [(1, (64, 56)); (2, (83, 78)); (3, (64, 80)); (4, (27, 21)); (5, (61, 83));
                             (6, (46, 7)); (7, (10, 96)); (8, (18, 59)); (9, (8, 78)); (10, (37, 68));
                             (11, (62, 59)); (12, (42, 7)); (13, (75, 19)); (14, (98, 80));
                             (15, (83, 37)); (16, (18, 16)); (17, (100, 95)); (18, (56, 43));
                             (19, (74, 68)); (20, (89, 46))])

let (robots9, shelters9) = ([("r1", (8, 76)); ("r2", (99, 97)); ("r3", (98, 100)); ("r4", (63, 15));
                             ("r5", (10, 91)); ("r6", (54, 11)); ("r7", (93, 58)); ("r8", (23, 2));
                             ("r9", (50, 41)); ("r10", (96, 65)); ("r11", (17, 20)); ("r12", (59, 82));
                             ("r13", (78, 85)); ("r14", (87, 97)); ("r15", (42, 93)); ("r16", (49, 82));
                             ("r17", (5, 27)); ("r18", (67, 0)); ("r19", (33, 46)); ("r20", (34, 53))],
                            [(1, (61, 62)); (2, (98, 4)); (3, (55, 14)); (4, (64, 30)); (5, (8, 90));
                             (6, (80, 41)); (7, (46, 31)); (8, (60, 7)); (9, (76, 86)); (10, (92, 85));
                             (11, (97, 39)); (12, (17, 11)); (13, (95, 15)); (14, (3, 3));
                             (15, (38, 25)); (16, (86, 40)); (17, (52, 10)); (18, (34, 93));
                             (19, (95, 72)); (20, (74, 5))])


let (robots10, shelters10) = ([("r1", (1, 12)); ("r2", (65, 17)); ("r3", (88, 22)); ("r4", (35, 73));
                               ("r5", (47, 70)); ("r6", (21, 96)); ("r7", (0, 85)); ("r8", (22, 32));
                               ("r9", (31, 63)); ("r10", (78, 22)); ("r11", (4, 94)); ("r12", (19, 70));
                               ("r13", (77, 6)); ("r14", (20, 92)); ("r15", (89, 15)); ("r16", (90, 90));
                               ("r17", (90, 73)); ("r18", (59, 97)); ("r19", (37, 30)); ("r20", (95, 51))],
                              [(1, (64, 56)); (2, (83, 78)); (3, (64, 80)); (4, (27, 21)); (5, (61, 83));
                               (6, (46, 7)); (7, (10, 96)); (8, (18, 59)); (9, (8, 78)); (10, (37, 68));
                               (11, (62, 59)); (12, (42, 7)); (13, (75, 19)); (14, (98, 80));
                               (15, (83, 37)); (16, (18, 16)); (17, (100, 95)); (18, (56, 43));
                               (19, (74, 68)); (20, (89, 46))])

let robots11 = [("r1", (1, 1)); ("r2", (1, 99))]
let shelters11 = [(3, (99, 99)); (4, (99, 1))]

let robots12 = [("r1", (3, 3))]
let shelters12 = [(5, (3, 72))]

let robots13 = [("r1", (0,0));("r2", (3,0))] 
let shelters13 = [(1, (3,3));(2, (0,3))]

let robots14 = [("r1", (3, 2));("r2", (3, 5)); ("r3", (3, 10))]
let shelters14 = [(1, (5, 10));(2, (5, 5));(3, (5, 2))]

let robots15 = [("r1", (3, 20)); ("r2", (10, 10)); ("r3", (20, 3)); ("r4", (50, 0))]
let shelters15 = [(1, (3, 0)); (2, (10, 20)); (3, (20, 0)); (4, (50,100))]

let robots16 = [("r1", (3, 20)); ("r2", (3, 12)); ("r3", (3, 10)); ("r4", (3, 5))]
let shelters16 = [(1, (5, 5)); (2, (5, 10)); (3, (5, 12)); (4, (5, 20))]

let robots17 = [("r1", (1, 0)); ("r2", (2, 0)); ("r3", (3, 0)); ("r4", (4, 0)); ("r5", (5, 0)); ("r6", (6, 0)); ("r7", (7, 0)); ("r8", (8, 0)); ("r9", (9, 0)); ("r10", (10, 0)); ("r11", (11, 0)); ("r12", (12, 0)); ("r13", (13, 0)); ("r14", (14, 0)); ("r15", (15, 0)); ("r16", (16, 0)); ("r17", (17, 0)); ("r18", (18, 0)); ("r19", (19, 0)); ("r20", (20, 0))] 
let shelters17 = [(1, (20, 20)); (2, (19, 20)); (3, (18, 20)); (4, (17, 20)); (5, (16, 20)); (6, (15, 20)); (7, (14, 20)); (8, (13, 20)); (9, (12, 20)); (10, (11, 20)); (11, (10, 20)); (12, (9, 20)); (13, (8, 20)); (14, (7, 20)); (15, (6, 20)); (16, (5, 20)); (17, (4, 20)); (18, (3, 20)); (19, (2, 20)); (20, (1, 20))]

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots1 shelters1 in
       hasIntersect robots1 shelters1 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots2 shelters2 in
       hasIntersect robots2 shelters2 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots3 shelters3 in
       hasIntersect robots3 shelters3 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots4 shelters4 in
       hasIntersect robots4 shelters4 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots5 shelters5 in
       hasIntersect robots5 shelters5 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots6 shelters6 in
       hasIntersect robots6 shelters6 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots7 shelters7 in
       hasIntersect robots7 shelters7 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots8 shelters8 in
       hasIntersect robots8 shelters8 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots9 shelters9 in
       hasIntersect robots9 shelters9 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots10 shelters10 in
       hasIntersect robots10 shelters10 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots11 shelters11 in
       hasIntersect robots11 shelters11 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots12 shelters12 in
       hasIntersect robots12 shelters12 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots13 shelters13 in
       hasIntersect robots13 shelters13 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots14 shelters14 in
       hasIntersect robots14 shelters14 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots15 shelters15 in
       hasIntersect robots15 shelters15 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots16 shelters16 in
       hasIntersect robots16 shelters16 ans = false)

let _ =
  output
    (fun () ->
       let ans = Duststorm.shelterAssign robots17 shelters17 in
       hasIntersect robots17 shelters17 ans = false)
