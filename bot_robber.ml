open Definition
open Registry
open Constant
open Util
open Structures
open Player
open Bot_general


(* worth of a piece, based only on its roll probability *)
let pc_worth pc hl : int =
	odds_of_roll ( roll_of (List.nth hl pc ) )


(* given a piece and a color, return if there are opponent colors on the piece *)
let opponents_on pc c il : bool =
	List.exists (fun x -> x != c) (colors_near pc il)
(* return if color c is on the piece pc *)
let color_on pc c il : bool =
	List.mem c (colors_near pc il)

(* helper. a list of all pieces that have at least one opponent on them *)
let opp_pieces c pieces il : piece list =
	List.filter ( fun piece -> opponents_on piece c il ) pieces
(* helper. a list of all pieces that do not include c *)
let not_us_pieces c pieces il : piece list =
	List.filter ( fun piece -> not (color_on piece c il) ) pieces

(* best pieces to place robber on, including pieces that color c may be on. 
   a list of pieces that have opponents on them, SORTED by the worth of the piece 
   does not include pieces that have the robber on them now *)
let best_rob_pieces1 c b : piece list =
	let (hl,_),(il,_),_,_,robbernow = b in
	List.filter (fun x -> x != robbernow)
			(sort_by (fun piece -> pc_worth piece hl) (opp_pieces c all_pieces il))
(* best pieces to place robber on, not including pieces that color c is on.
   a list of pieces that have opponents on them BUT NOT color c, SORTED by the worth of the piece 
   does not include pieces that have the robber on them now. *)
let best_rob_pieces2 c b : piece list =
	let il = il_of b in
	not_us_pieces c (best_rob_pieces1 c b) il


(* choose the player with the most resources from playerlist pl,
   excluding the player with color c *)
let rec most_resources_excluding c pl : color option =
	match sort_by (fun player -> sum_cost (inv_of player) ) pl with
	| [] -> None
	| h::t when (color_of h = c) -> most_resources_excluding c t
	| h::t -> Some (color_of h)

(* choose the color that color c should steal from if the robber 
   is placed at piece pc *)
let best_steal_from pc c il pl : color option =
	let colors = colors_near pc il in
	let steal_list = List.map (fun color -> player color pl) colors in
	most_resources_excluding c steal_list