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
	let len = longest_road c rl il in
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
