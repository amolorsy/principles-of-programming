type design = TURTLE | WAVE | DRAGON    (* three design patterns *)
type orientation = NW | NE | SE | SW
type box = BOX of orientation * design | GLUED of box * box * box * box

module type FRAME =
sig
  val box: box
  val rotate: box -> box                (* roatate box M to 3 to W to E *)
  val pp: box -> int * int -> unit      (* pretty printer *)
  val size: int
end

module BasicFrame (Design: sig val design: design end): FRAME = 
struct 
  exception NON_BASIC_BOX
  let box = BOX (NW, Design.design)     (* a box is defined *)
  let rec rotate (b:box) : box =
  	match b with
	| BOX (o, e) ->
		(match o with
		| NW -> BOX (NE, e)
		| NE -> BOX (SE, e)
		| SE -> BOX (SW, e)
		| SW -> BOX (NW, e))
	| GLUED (nw, ne, se, sw) -> raise NON_BASIC_BOX

  let pp b center = 
    match b with
    | BOX (NW,x) -> print_int 0                  (* dummy, fill it if you want *)
    | BOX (NE,x) -> print_int 0                  (* dummy, fill it if you want *)
    | BOX (SE,x) -> print_int 0                  (* dummy, fill it if you want *)
    | BOX (SW,x) -> print_int 0                  (* dummy, fill it if you want *)
    | _ -> raise NON_BASIC_BOX
  
  let size = 1
end

module Rotate (Box:FRAME) : FRAME =
struct 
  let box = Box.rotate Box.box
  let rotate (b:box) : box = Box.rotate b
  let pp b center = print_int 0                  (* dummy, fill it if you want *)
  let size = Box.size
end

module Glue (Nw:FRAME) (Ne:FRAME) (Se:FRAME) (Sw:FRAME) : FRAME =
struct
  exception DIFFERENT_SIZED_BOXES

  let is_size_eq =
  	if (Nw.size = Ne.size) && (Ne.size = Se.size) && (Se.size = Sw.size) then true else false
  
  let box =
  	match is_size_eq with
	| true -> GLUED (Nw.box, Ne.box, Se.box, Sw.box)
	| false -> raise DIFFERENT_SIZED_BOXES

  let rec rotate b =
  	match is_size_eq with
	| true ->
		(match b with
		| BOX (o, e) ->
			(match o with
			| NW -> BOX (NE, e)
			| NE -> BOX (SE, e)
			| SE -> BOX (SW, e)
			| SW -> BOX (NW, e))
		| GLUED (nw, ne, se, sw) ->
			GLUED (rotate sw, rotate nw, rotate ne, rotate se))
	| false -> raise DIFFERENT_SIZED_BOXES

  let pp b center = print_int 0

  let size =
  	match is_size_eq with
	| true -> Nw.size + Ne.size + Se.size + Sw.size
	| false -> raise DIFFERENT_SIZED_BOXES
end
