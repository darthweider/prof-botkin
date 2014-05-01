open Definition
open Constant
open Util
open Print
open Player


let indexed_intersections il : (int * intersection) list =
	let _, indexed = List.fold_right ( fun i (ct,iacc) -> ct+1, (ct, i)::iacc ) il (0, []) in
	indexed

let indexed_hexs hl : (int * hex) list =
	let _, indexed = List.fold_right ( fun h (ct,hacc) -> ct+1, (ct+1, h)::hacc ) hl (0, []) in
	indexed

(* add_road c l rl adds a road on line l for color c to the road list rl.
   Fails if a road already exists on l *)
let add_road c ln rl : road list =
	if List.exists (fun (_,(pt1, pt2)) -> (pt1,pt2) = ln || (pt2,pt1) = ln ) rl
	then failwith "A road already exists on this line."
	else (c, ln)::rl

(* add a town for color c at point pt on intersection list il.
   Raises an exception if there is a previous settlement at that point or
   if the surrounding area is not clear of settlements *)
let add_town c pt il : intersection list =
	let empty_land = pt :: (adjacent_points pt) in
	let _, il' = List.fold_right (fun i (ct,ilacc) ->
		match i with
		| Some(s) when List.mem ct empty_land ->
		    failwith "Cannot place town here. Area is populated by an existing settlement."
		| None when ct = pt -> ( ct+1, Some (c,Town) :: ilacc )
		| _                 -> ( ct+1, i::ilacc ))
		il (0,[]) in
	il'


let num_towns_of c il : int =
	list_count ( fun i -> 
		match i with 
		| Some(color,s) when color = c -> true 
		| _ -> false ) il

let num_towns_total pl il : int =
	List.fold_left (fun nacc p -> (num_towns_of (color_of p) il) + nacc) 0 pl


let initial c (pt1,pt2) b : board =
	let m, (il, rl), dk, dis, rob = b in
	let structures' = try ( (add_town c (pt1) il), (add_road c (pt1, pt2) rl) ) 
		              with _ -> failwith "Failed to place initial road and settlement." in
	(m, structures', dk, dis, rob)

let valid_initial c ln b : bool =
	try (fun x -> true) (initial c ln b)
	with _ -> false

let rec random_initialmove c b : move =
	let random_ln = (Random.int 53, Random.int 53) in
	if valid_initial c random_ln b then InitialMove(random_ln)
	else random_initialmove c b


(* If c can built a road on line *)
let valid_build_road c ln =
	failwith "riverrun, past Eve and Adam's "
	(* not an existing road: not in board's structure's road list *)
	(* adjacent to a road or town of this player *)
	(* have not exceeded max roads per player *)
let valid_build_town c pt =
	failwith "riverrun, past Eve and Adam's "
	(* no existing settlement *)
	(* no adjacent settlement. use adjacent_points *)
	(* color has a road at pt *)
	(* have not exceeded max towns per player *)
let valid_build_city c pt =
	failwith "riverrun, past Eve and Adam's "
	(* c already has a town at pt *)
	(* have not exceeded max cities per player *)