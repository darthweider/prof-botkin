open Definition
open Constant
open Util
open Print
open Printf
open Player
open Structures

(* returns the first color in player list pl that saitsifies predicate pred *)
let color_that pred pl : color option =
	try Some ( color_of (List.find pred pl) )
	with Not_found -> None
(* the color that currently owns the longest road trophy *)
let longest_road_owner pl : color option = color_that has_longest_road pl
(* the color that curently owns the largest army *)
let largest_army_owner pl : color option = color_that has_largest_army pl

(* gives the longest road trophy to color c, and removes the trophy
   from all other players *)
let give_longest_road_to c pl : player list =
	List.map ( fun p ->
		let (col,h,(k,longest,largest)) = p in 
		if col = c
			then (col,h,(k,true,largest))
		else (col,h,(k,false,largest))) pl

let give_largest_army_to c pl : player list =
	List.map ( fun p ->
		let (col,h,(k,longest,largest)) = p in 
		if col = c 
			then (col,h,(k,longest,true))
		else (col,h,(k,longest,false))) pl

(* if color c now has the longest road, update the trophies of c accordingly *)
let update_longest_road c rl il pl : player list =
	let len = longest_road c (roads_of c rl) il in
	print_string (string_of_list (fun (_, (a, b)) -> (string_of_int a) ^ " " ^ (string_of_int b)) rl);
	if len >= cMIN_LONGEST_ROAD
		then match longest_road_owner pl with
		| Some(prev_owner) when len > longest_road prev_owner rl il -> give_longest_road_to c pl
		| Some(prev_owner)                                          -> pl
		| None                                                      -> give_longest_road_to c pl
	else                                                               pl

let update_largest_army c pl : player list =
	let k = num_knights (player c pl) in
	if k >= cMIN_LARGEST_ARMY
		then match largest_army_owner pl with
		| Some(prev_owner) when k > num_knights (player prev_owner pl) -> give_largest_army_to c pl
		| Some(prev_owner)                                             -> pl
		| None                                                         -> give_largest_army_to c pl
	else                                                                  pl




(* Victory Points that color c is currently holding *)
let num_victory_cards c pl : int =
	let hand = cards_of (player c pl) in
	List.fold_left ( fun vps card -> 
		match card with
		| VictoryPoint -> vps + 1
		| _ -> vps )
		0 (reveal hand)

(* if color c has won *)
let has_won c il pl : bool =
	let card_pts = cVP_CARD * num_victory_cards c pl in
	let town_pts = cVP_TOWN * num_towns_of c il in
	let city_pts = cVP_CITY * num_cities_of c il in
	let road_pts = if has_longest_road (player c pl) then cVP_LONGEST_ROAD else 0 in
	let army_pts = if has_largest_army (player c pl) then cVP_LARGEST_ARMY else 0 in
	card_pts + town_pts + city_pts + road_pts + army_pts > cWIN_CONDITION