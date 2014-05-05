open Definition
open Constant
open Util
open Print
open Printf
open Player


let indexed (l : 'a list) : (int * 'a) list =
	let _, indexed = List.fold_left ( fun (ct,lacc) x -> ct+1, lacc @ [(ct, x)] ) (0, []) l in
	indexed



(*==================ROADS======================*)

(*Given the list of all roads placed and a color c, returns a list of all the roads of color c*)
let roads_of c roadl = 
	List.filter (fun (col,_) -> col = c) roadl

(*Given a list of roads and a color, returns the list of roads that could potential (or have already been) placed by a single player*)
let all_possible_roads (roadl : road list) (c : color) : road list =
	let rec helper roadl acc = 
		match roadl with
		| (_,(beg,e))::tl ->
			(*make a list of lines, denoted as (beg, e) tuples*)
			let adjstart = List.map (fun x -> (c,(beg, x))) (adjacent_points beg) in
			let adjend = List.map (fun x -> (c,(e, x))) (adjacent_points e) in
			(*merge the two lists. Duplicates will not mattter. Recurse*)
			helper tl (adjstart@adjend@acc)
		| _ -> acc in 
	helper roadl []

(* the color who currently owns the longest road trophy *)
let longest_road_owner pl : color option =
	try Some ( color_of (List.find has_longest_road pl) )
	with Not_found -> None

(* gives the longest road trophy to color c, and removes the trophy
   from all other players *)
let give_longest_road_to c pl : player list =
	List.map ( fun p ->
		let (c,h,(k,longest,largest)) = p in 
		if color_of p = c 
			then (c,h,(k,true,largest))
		else (c,h,(k,false,largest))) pl

(* if color c now has the longest road, update the trophies of c accordingly *)
let update_longest_road c rl pl : player list =
	let len = List.length (roads_of c rl) in
	if len >= cMIN_LONGEST_ROAD
		then match longest_road_owner pl with
		| Some(prev_owner) when len > List.length (roads_of prev_owner rl)
		                     -> give_longest_road_to c pl
		| Some(prev_owner)   -> pl
		| None               -> give_longest_road_to c pl
	else                        pl



(*In this method we assume that rd is a valid road placement. Places the road*)
let add_road rd rl : road list =
	(*let c,(pt1,pt2) = rd in*)
	rd::rl
	(* print (sprintf "road at %i and %i" pt1 pt2); *)

(* add_road c l rl adds a road on line l for color c to the road list rl.
   Fails if a road already exists on l *)
let initial_add_road c ln rl : road list =
	let pt1,pt2 = ln in
	if not (List.mem pt1 (adjacent_points pt2)) ||
		List.exists (fun (_,(rpt1, rpt2)) -> (rpt1,rpt2) = ln || (rpt2,rpt1) = ln ) rl
		then failwith "Cannot make a road here."
	else (c, ln)::rl
	(* print (sprintf "road at %i and %i" pt1 pt2); *)

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
		| None when loc = pt ->  ilacc @ [Some (c,Town)]
		| _                  ->  ilacc @ [i] )
		[] indexed_intersections

(*Adds a city at the given index. Assumes that this is a valid move (does not check if it is not)*)
let add_city c pt il : intersection list = 
	let indexed_intersections = indexed il in
	List.fold_left (fun ilacc (loc, i) -> 
		if loc = pt then ilacc @ [Some(c, City)]
		else ilacc @ [i]) [] indexed_intersections

(* adds an initial road and an initial town *)
let initial c (pt1,pt2) b : board =
	let m, (il, rl), dk, dis, rob = b in
	let structures' = try ( (add_town c (pt1) il), (initial_add_road c (pt1, pt2) rl) ) 
		              with _ -> failwith "Failed to place initial road and settlement." in
	(m, structures', dk, dis, rob)



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

let num_cities_of c il : int =
	list_count (fun i -> 
		match i with
		| Some(col, s) when s = City && col = c-> true
		| _ -> false) il

let num_towns_total pl il : int =
	List.fold_left (fun nacc p -> (num_towns_of (color_of p) il) + nacc) 0 pl



(* If c can built a road on line *)
let valid_build_road c pl desiredr roadl =
	let croads = roads_of c roadl in
	let targetcol, (_,_) = desiredr in
	let p = player c pl in
	(*if target color = c AND if player can afford to build a road AND if a road does not exist at the desired location AND number of roads < max number of roads*)
	if c=targetcol 
		&& (can_pay p cCOST_ROAD) 
		&& List.length (List.filter (fun x -> (snd x) = (snd desiredr)) roadl) = 0 
		&& List.length croads < cMAX_ROADS_PER_PLAYER then
		begin
			(*check if road is valid*)
			let poss_roads = all_possible_roads croads c in
			List.length (List.filter (fun x -> (snd x) = (snd desiredr)) poss_roads) <> 0
		end
	else false
	(* not an existing road: not in board's structure's road list *)
	(* adjacent to a road or town of this player--This is the same as adjacent to a road of this player *)
	(* have not exceeded max roads per player *)
let valid_build_town c pt pl roadl il=
	let p = player c pl in
	let croads = roads_of c roadl in
	let pathexists = List.exists (fun (col, (s, e)) -> s=pt || e=pt) croads in
	let adj = adjacent_points pt in
	(*Is the target settlement empty, and are there no adjacent settlements?*)
	let empty_in_and_around = List.for_all (fun x -> (List.nth il x) = None) (pt::adj) in
	(*we can afford a town AND we have not exceeded max # of towns AND the target+adjacent squares=empty AND road leads to point*)
	can_pay p cCOST_TOWN && num_towns_of c il < cMAX_TOWNS_PER_PLAYER && empty_in_and_around && pathexists

let valid_build_city (c : color) (pt : point) (pl : player list) (il : intersection list) : bool =
	let p = player c pl in
	match List.nth il pt with
	| Some (col, s) when s=Town ->
			(*Town is of correct color AND we can afford a city AND we have not exceeded max num of cities*)
			 col = c && can_pay p cCOST_CITY && (num_cities_of c il) < cMAX_CITIES_PER_PLAYER
	| _ -> false

let valid_initial c ln b : bool =
	try (fun x -> true) (initial c ln b)
	with _ -> false