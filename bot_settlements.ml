open Definition
open Registry
open Constant
open Util

(* a list of all existing points, 0 to cMAX_POINT_NUM 
   For iterating *)
let all_pts : int list =
	let rec nlist n acc : int list =
		if n = 0 then acc
		else nlist (n-1) (n :: acc) in
	nlist cMAX_POINT_NUM []

(* a list of all unoccupied points on the map *)
let available_pts intersections : int list =
	List.fold_left ( fun availables pt -> 
	match List.nth intersections pt with
	| Some(c,s) -> availables
	| None -> pt::availables ) [] all_pts

(* the odds of rolling n. Returns the numerator of the probability (denominator is 36 *)
let odds_of_roll n =
	match n with
	| 2 | 12 -> 1
	| 3 | 11 -> 2
	| 4 | 10 -> 3
	| 5 | 9 -> 4
	| 6 | 8 -> 5
	| 7 -> 6
	| _ -> 0

(* the simple worth of placing a settlement at a given point pt.
   the sum of the odds of adjacent hexes *)
let worth_of pt (hexl, _) : int =
	let adj_hexes = List.map (fun x -> List.nth hexl x) (adjacent_pieces pt) in 
	List.fold_left (fun worth (_,roll) -> worth + odds_of_roll roll ) 0 adj_hexes

(* compares the simple worth of points 1 and 2. *)
let compare_worth map pt1 pt2 : int =
	let w1 = worth_of pt1 map in
	let w2 = worth_of pt2 map in
	if w1 > w2 then 1
	else if w1 < w2 then ~-1
	else 0

 (* from a list of points, sorts them in order of simple worth *)
let best_pts pts map : int list =
	List.sort (compare_worth map) pts

(* a list of all available points, sorted in order of simple worth *)
let best_available_pts map intersections : int list =
	best_pts (available_pts intersections) map

