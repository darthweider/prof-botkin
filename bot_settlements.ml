open Definition
open Registry
open Constant
open Util
open Structures
open Bot_general


(* the simple worth of placing a settlement at a given point pt.
   the sum of the odds of adjacent hexes *)
let pt_worth pt (hexl, _) : int =
	let adj_hexes = List.map (fun x -> List.nth hexl x) (adjacent_pieces pt) in 
	List.fold_left (fun worth hex -> worth + odds_of_roll (roll_of hex) ) 0 adj_hexes

(* compares the simple worth of points 1 and 2. *)
let compare_worth map pt1 pt2 : int =
	let w1 = pt_worth pt1 map in
	let w2 = pt_worth pt2 map in
	if w1 < w2 then 1
	else if w1 > w2 then ~-1
	else 0

 (* from a list of points, sorts them in order of simple worth *)
let best_pts pts map : int list =
	List.sort (compare_worth map) pts

(* a list of all available points, sorted in order of simple worth *)
let best_available_pts_on_map b : int list =
	let map, (il, _),_,_,_ = b in
	best_pts (all_available_pts il) map

(* best point for color c to build a town at, based on simple worth. 
   Returns none if there is nowhere for c to build a town at right now. *)
let best_build_town_now c b : int option =
	let map, (il, rl),_,_,_ = b in
	let pts = road_pts_of c rl in
	match best_pts pts map with
	| [] -> None
	| h::t -> Some(h)




let best_build_city_now c b : int option =
	failwith ""