open Definition
open Constant
open Util
open Print
open Printf
open Player


let indexed (l : 'a list) : (int * 'a) list =
	let _, indexed = List.fold_right ( fun x (ct,lacc) -> ct+1, (ct, x)::lacc ) l (0, []) in
	indexed


(* add_road c l rl adds a road on line l for color c to the road list rl.
   Fails if a road already exists on l *)
let add_road c ln rl : road list =
	let pt1,pt2 = ln in
	if not (List.mem pt1 (adjacent_points pt2)) || 
	       List.exists (fun (_,(rpt1, rpt2)) -> (rpt1,rpt2) = ln || (rpt2,rpt1) = ln ) rl
	then failwith "Cannot make a road here."
	else print (sprintf "road at %i and %i" pt1 pt2); (c, ln)::rl

(* add a town for color c at point pt on intersection list il.
   Raises an exception if there is a previous settlement at that point or
   if the surrounding area is not clear of settlements *)
let add_town c pt il : intersection list =
	let empty_land = pt :: (adjacent_points pt) in
	let indexed_intersections = indexed il in

	List.fold_left (fun ilacc (loc, i) ->
		match i with
		| Some(s) when List.mem loc empty_land ->
		    failwith "Cannot place town here. Area is populated by an existing settlement."
		| None when loc = pt ->  Some (c,Town) :: ilacc)
		| _                  ->  i::ilacc )
		[] indexed_intersections


let initial c (pt1,pt2) b : board =
	let m, (il, rl), dk, dis, rob = b in
	let structures' = try ( (add_town c (pt1) il), (add_road c (pt1, pt2) rl) ) 
		              with _ -> failwith "Failed to place initial road and settlement." in
	(m, structures', dk, dis, rob)

let valid_initial c ln b : bool =
	try (fun x -> true) (initial c ln b)
	with _ -> false

let rec random_initialmove c b : move =
	let rand_pt1 = Random.int 53 in
	let rand_pt2 = get_some (pick_random (adjacent_points rand_pt1)) in
	let rand_ln = (rand_pt1, rand_pt2) in
	if valid_initial c rand_ln b then InitialMove(rand_ln)
	else random_initialmove c b


let num_towns_of c il : int =
	list_count ( fun i -> 
		match i with 
		| Some(color,s) when color = c -> true 
		| _ -> false ) il

let num_towns_total pl il : int =
	List.fold_left (fun nacc p -> (num_towns_of (color_of p) il) + nacc) 0 pl

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