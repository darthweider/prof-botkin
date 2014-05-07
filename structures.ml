open Definition
open Constant
open Util
open Print
open Printf
open Player


let indexed (l : 'a list) : (int * 'a) list =
	let _, indexed = List.fold_left ( fun (ct,lacc) x -> ct+1, lacc @ [(ct, x)] ) (0, []) l in
	indexed

(* creates an int list counting from 0 to n, inclusive *)
let rec nlist n acc : int list =
	if n = 0 then acc
	else nlist (n-1) (n :: acc)



(*================BOARD INFORMATION===============*)
let hl_of ((hl,_),_,_,_,_) : hex list = hl
let portl_of ((_,portl),_,_,_,_ ) : port list  = portl
let il_of (_,(il,_),_,_,_) : intersection list = il
let rl_of (_,(_,rl),_,_,_) : road list = rl
let dk_of (_, _, dk, _, _) : deck = dk


let valid_point pt =
	0 <= pt && pt <=cMAX_POINT_NUM

let valid_pc pc = 
	0 <= pc && pc <=cMAX_PIECE_NUM

let valid_line (pt1,pt2) =
	valid_point pt1 && valid_point pt2 && List.mem pt1 (adjacent_points pt2)

(* there is no settlement on a given pt, or immediately adjacent to it *)
let area_free pt il : bool =
	let adj = adjacent_points pt in
	List.for_all (fun x -> (List.nth il x) = None) (pt::adj)

(* a list of all existing points. For iterating over *)
let all_pts : int list =
	nlist cMAX_POINT_NUM []
let all_pieces : int list = 
	nlist cMAX_PIECE_NUM []

(* a list of all unoccupied points on the map whose areas are free *)
let all_available_pts il : int list =
	List.fold_left ( fun available pt -> 
	match List.nth il pt with
	| None when (area_free pt il) -> pt::available 
	| _ -> available ) [] all_pts

(* list of colors next to a given piece, according to intersection list il *)
let colors_near (pc : piece) (il : intersection list) : color list =
	let adj = piece_corners pc in 
	List.fold_left (fun clist pt -> 
		match List.nth il pt with
		| Some(c,settle) -> c::clist
		| None -> clist )
		[] adj


(*===========HEXES===============*)

let roll_of (t,r) : roll = r


(*==================ROADS======================*)

(*Given the list of all roads placed and a color c, returns a list of all the roads of color c*)
let roads_of c roadl = 
	List.filter (fun (col,_) -> col = c) roadl

(* A road has been built on line ln according to road list rl *)
let exists_road ln rl =
	List.exists (fun (_,(rpt1, rpt2)) -> (rpt1,rpt2) = ln || (rpt2,rpt1) = ln ) rl


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


(* a list of points that are occupied by roads of color c *)
let road_pts_of c rl =
	List.fold_left ( fun owned (_,(pt1,pt2)) -> 
		match List.mem pt1 owned, List.mem pt2 owned with
		| true, true -> owned
		| true, false -> pt2::owned 
		| false, true -> pt1::owned 
		| false, false -> pt1::pt2::owned ) [] (roads_of c rl)


(*==================SETTLEMENTS=====================*)
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

(* points at which c has a town *)
let town_pts_of c il : int list =
	let indexed_intersections = indexed il in
	List.fold_left ( fun towns (n, i) ->
		match i with
		| Some(color,Town) when color = c -> n::towns 
		| _ -> towns ) [] indexed_intersections



(*=====================VALIDATING FOR BUILDING=================*)

(* If c can built a road on line. The road cost "cost"--that way we can recycle function for placing free roads *)
let valid_build_road c pl desiredr roadl il cost=
	let (targetcol,(s, e)) = desiredr in
	let croads = roads_of c roadl in
	let p = player c pl in
	(*if target color = c AND if player can afford to build a road AND if a road does not exist at the desired location AND number of roads < max number of roads*)
	if c=targetcol 
		&& (can_pay p cost) 
		&& not (List.exists (fun (c, (a,b)) -> (a = s && b = e) || (a = e && b = s)) roadl) 
		&& List.length croads < cMAX_ROADS_PER_PLAYER
		&& valid_point s && valid_point e then
		begin
			(*check if road is valid*)
			let poss_roads = all_possible_roads croads c in
			(*Check end points to see if we're trying to build over an enemy settlement. If we are, check that the other end of our road
				meets with another one of our roads*)
			let not_interfering = match (List.nth il s), (List.nth il e) with
				| (Some(col, _), _) when c <> col-> (List.exists (fun (_, (pt1, pt2)) -> pt1 = e || pt2 = e) croads)
				| (_, Some(col, _)) when c <> col-> (List.exists (fun (_, (pt1, pt2)) -> pt1 = s || pt2 = s) croads)
				| _ -> true in
			List.length (List.filter (fun (_,(a,b)) -> (a = s && b = e) || (a = e && b = s)) poss_roads) <> 0 && not_interfering
		end
	else false
	(* not an existing road: not in board's structure's road list *)
	(* adjacent to a road or town of this player--This is the same as adjacent to a road of this player *)
	(* have not exceeded max roads per player *)
	let valid_build_town c pt pl roadl il=
	let p = player c pl in
	let croads = roads_of c roadl in
	let pathexists = List.exists (fun (col, (s, e)) -> s=pt || e=pt) croads in
	if valid_point pt then begin
		(*Is the target settlement empty, and are there no adjacent settlements?*)
		(*we can afford a town AND we have not exceeded max # of towns AND the target+adjacent squares=empty AND road leads to point*)
		can_pay p cCOST_TOWN && num_towns_of c il < cMAX_TOWNS_PER_PLAYER && (area_free pt il) && pathexists	
	end
	else false


let valid_build_city (c : color) (pt : point) (pl : player list) (il : intersection list) : bool =
	let p = player c pl in
	if valid_point pt then begin
		match List.nth il pt with
		| Some (col, s) when s=Town ->
				(*Town is of correct color AND we can afford a city AND we have not exceeded max num of cities*)
				 col = c && can_pay p cCOST_CITY && (num_cities_of c il) < cMAX_CITIES_PER_PLAYER
		| _ -> false
	end
	else false

(*Verify that the road building card can be played*)
let valid_road_building (c: color) (pl: player list) (rl : road list) (il : intersection list) (road1 : road) (road2 : road option) = 
	let firstvalid = valid_build_road c pl road1 rl il (0,0,0,0,0) in
	(*If we're only building one road*)
	if is_none road2 then begin
		firstvalid && valid_play_card RoadBuilding c pl
	end
	(*If we're building both roads*)
	else (
		let road2 = get_some road2 in
		let secondvalid = valid_build_road c pl road2 rl il (0,0,0,0,0) in
		match (firstvalid, secondvalid) with
		| true, false -> begin
			(*Check if placing first road makes second road valid*)
			let rl' = road1::rl in
			(valid_play_card RoadBuilding c pl) &&valid_build_road c pl road2 rl' il (0,0,0,0,0)
		end
		| false, true -> begin
			(*Check if placing second road makes first road valid*)
			let rl' = road2::rl in
			(valid_play_card RoadBuilding c pl) && valid_build_road c pl road1 rl' il (0,0,0,0,0)
		end
		| true, true ->begin
			(*We pick a valid road (either in this case) and see if we can place both*)
			let rl' = road2::rl in
			(valid_play_card RoadBuilding c pl) && valid_build_road c pl road1 rl' il (0,0,0,0,0)
		end
		| _ -> false
	)


let valid_initial c ln b : bool =
	let _,(il,rl),_,_,_ = b in
	let pt1,pt2 = ln in
	(valid_line ln) && (not (exists_road ln rl)) && (area_free pt1 il)


(*==================BUILDING FUNCTIONS==============*)
let add_road rd rl = 
	rd::rl

(* add a town for color c at point pt on intersection list il.
   Assumes this is a valid move *)
let add_town c pt il : intersection list =
	let indexed_intersections = indexed il in

	List.fold_left (fun ilacc (loc, i) ->
		match i with
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
	let structures' = try ( (add_town c (pt1) il), ( (c, (pt1, pt2))::rl) ) 
		              with _ -> failwith "Failed to place initial road and settlement." in
	(m, structures', dk, dis, rob)



(*=================RANDOM MOVE================*)
let random_pt : point = Random.int cNUM_POINTS
let random_adj_pt pt1 : point = get_some (pick_random (adjacent_points pt1))
let random_line : line =
	let pt1 = random_pt in
	pt1, random_adj_pt pt1

let rec random_initialmove c b : move =
	let rand_ln = random_line in
	if valid_initial c rand_ln b then InitialMove(rand_ln)
	else random_initialmove c b