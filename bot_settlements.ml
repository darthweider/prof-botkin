open Definition
open Registry
open Constant
open Util
open Structures
open Bot_general


(* the simple worth of placing a settlement at a given point pt.
   the sum of the odds of adjacent hexes *)
let pt_worth pt b : int =
	let hexl = hl_of b in
	let adj_hexes = List.map (fun x -> List.nth hexl x) (adjacent_pieces pt) in 
	List.fold_left (fun worth hex -> worth + odds_of_roll (roll_of hex) ) 0 adj_hexes

(* DEPRACATED 
(* compares the simple worth of points 1 and 2. *)
let compare_worth b pt1 pt2 : int =
	let w1 = pt_worth pt1 b in
	let w2 = pt_worth pt2 b in
	if w1 < w2 then 1
	else if w1 > w2 then ~-1
	else 0*)

 (* from a list of points, sorts them in order of simple worth *)
let best_pts pts b : int list =
	sort_by (fun pt -> pt_worth pt b) pts

(* a list of all available points, sorted in order of simple worth *)
let best_available_pts_on_map b : int list =
	let il = il_of b in
	best_pts (all_available_pts il) b

(* best point for color c to build a town at, based on simple worth. 
   Returns none if there is nowhere for c to build a town at right now. *)
let best_build_town_now c b : int option =
	let rl = rl_of b in
	let il = il_of b in
	let pts = road_pts_of c rl in
	let bst = List.filter (fun pt -> area_free pt il) pts in
	match bst with
	| [] -> None
	| h::t -> Some(h)

(* best place to build a city for color c, based on simple worth of its towns *)
let best_build_city_now c b : int option =
	let il = il_of b in
	let towns = town_pts_of c il in
	let l = List.filter ( fun pt -> List.mem pt towns ) (best_pts all_pts b) in
	match l with
	| [] -> None
	| h::t -> Some(h)



(*===========INITIALIZING============*)
  let rec handle_initial cm b : move =
    let tentative_ln = 
      match best_available_pts_on_map b with
      | best1::t -> (best1, random_adj_pt best1)
      | [] -> random_line in
    if valid_initial cm tentative_ln b then InitialMove(tentative_ln)
    else handle_initial cm b

let handle_city c b = 
	try (Action(BuyBuild(BuildCity(get_some (best_build_city_now c b)))))
		with _ -> failwith "Incorrectly handled a city"

let handle_town c b = 
	try (Action(BuyBuild(BuildTown(get_some (best_build_town_now c b)))))
		with _ -> failwith "Incorrectly handled a town"