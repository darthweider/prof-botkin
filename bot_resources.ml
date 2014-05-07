open Definition
open Registry
open Constant
open Util
open Structures
open Player
open Robber
open Bot_general


(* returns the highest resource in an inventory.
   PRE: there are resources in inv. *)
let highest_resource inv : resource =
	let b,w,o,g,l = inv in
	let sorted_costs = List.rev (List.sort (compare) (b::w::o::g::[l])) in
	let highest = List.hd sorted_costs in
	     if highest = w then Wool
	else if highest = g then Grain
	else if highest = o then Ore
	else if highest = l then Lumber
	else                     Brick

let lowest_resource inv : resource =
	let b,w,o,g,l = inv in
	let sorted_costs = List.sort (compare) (b::w::o::g::[l]) in
	let lowest = List.hd sorted_costs in
	     if lowest = b then Brick
	else if lowest = l then Lumber
	else if lowest = o then Ore
	else if lowest = g then Grain
	else                    Wool


(*================ROBBER==================*)

(* worth of a piece, based only on its roll probability *)
let pc_worth pc hl : int =
	odds_of_roll ( roll_of (List.nth hl pc ) )

(* given a piece and a color, return if there are opponent colors on the piece *)
let opponents_on pc c il : bool =
	List.exists (fun x -> x <> c) (colors_near pc il)
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
	List.filter (fun x -> x <> robbernow)
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


let handle_robber cm b pl : move =
	let il = il_of b in
	match best_rob_pieces2 cm b with
	| pc::t -> RobberMove(pc, (best_steal_from pc cm il pl) )
	| [] ->
	  match best_rob_pieces1 cm b with
	  | pc::t -> RobberMove(pc, (best_steal_from pc cm il pl) )
	  | [] -> random_rob cm b


(*==============TRADE RESPONSE================*)

(* a trade in which player1 gives player2 cost1 for cost2 resources is favorable
   for player 2 when the total number of resources player 2 receives is greater than
   it gives. *)
let favorable_trade cost1 cost2 =
	(sum_cost cost1) > (sum_cost cost2)

(* number of cards that color c has *)
let ncards c pl =
	match cards_of (player c pl) with
	| Hidden(n) -> n
	| Reveal(cardl) -> List.length cardl

(* estimate of the VPs of color c *)
let vp_estimate c il pl : int =
	let town_pts = cVP_TOWN * num_towns_of c il in
	let city_pts = cVP_CITY * num_cities_of c il in
	let road_pts = if has_longest_road (player c pl) then cVP_LONGEST_ROAD else 0 in
	let army_pts = if has_largest_army (player c pl) then cVP_LARGEST_ARMY else 0 in
	let card_pts = cVP_CARD * (ncards c pl) / 2 in
	town_pts + city_pts + road_pts + army_pts + card_pts

(* a trade is acceptable *)
let okay_trade active c cost1 cost2 il pl =
	can_pay (player c pl) cost2 &&
	favorable_trade cost1 cost2 &&
	(vp_estimate active il pl) < (cWIN_CONDITION - 1)

let handle_trade active pendingtrade il pl =
  match pendingtrade with
  | Some(c,cost1,cost2) when okay_trade active c cost1 cost2 il pl -> TradeResponse(true)
  | _ -> TradeResponse(false)


(*===================MARITIME TRADE==============*)

(* returns if color c will have at least two leftover of a resource after paying n of it *)
let resources_leftover n res c pl : bool =
	let min_cost = n_resource_cost res (n+2) in
	can_pay (player c pl) min_cost

(* if a maritime trade should be conducted *)
let should_maritime c pl b : bool =
	let inv = inv_of (player c pl) in
	let ports = get_ports c (il_of b) (portl_of b) in

	let high_res = highest_resource inv in
	let rate = best_trade_rate ports high_res in
	resources_leftover rate high_res c pl

(* conducts a maritime trade 
   PRE the maritime trade is valid: color c can pay it *)
let handle_maritime c pl : move =
	let inv = inv_of (player c pl) in

	let high_res = highest_resource inv in
	let low_res = lowest_resource inv in
	Action ( MaritimeTrade (high_res, low_res) )


(*===============DISCARD================*)

(* cost to discard from inv, when n resources need to be discarded, 
   discards the resources that are most abundant in inv *)
let discardn n inv : cost =
	let rec helper n inv chosen =
		match n with
		| 0 -> chosen
		| _ -> let (chose : cost) = single_resource_cost (highest_resource inv) in
			   helper (n-1) (diff_cost inv chose) (add_cost chosen chose) in
	helper n inv empty_cost

(* cost of discarding half of the resources of color c. 
   discards most abundant resources first *)
let discard_half c pl : cost =
	let inv = inv_of (player c pl) in
	let n = (sum_cost (inv)) / 2 in
	discardn n inv
		

(*==========TRADE INITIATION===========*)

(* the cost that inventory inv can currently contribute towards building something of buildcost *)
let have_for buildcost inv : cost =
	map_cost2 ( fun a b -> min a b ) buildcost inv

(* returns the cost that inventory inv still needs to build something of buildcost *)
let need_for buildcost inv : cost =
	floor_diff_cost buildcost inv

(* the cost that the inventory can trade in order to accumulate resources for something of buildcost *)
let free_for_trade buildcost inv : cost = diff_cost inv (have_for buildcost inv)

(* the inventory is one resource away from building something of buildcost *)
let one_away buildcost inv : bool =
	sum_cost (need_for cCOST_CITY inv) = 1 


(* compares two players p1 and p2 to see which is a better trading partner *)
let better_partner scores p1 p2 =
	let s1, s2 = Hashtbl.find scores (color_of p1), Hashtbl.find scores (color_of p2) in
	if s1 < s2 then 1
	else if s1 > s2 then ~-1
	else 0

(* players that have a given cost available *)
let can_ask_for cost pl : player list =
	List.fold_left ( fun ask p ->
		if (can_pay p cost) then p::ask
		else ask ) [] pl

(* players that should be asked for a trade to get cost, in order of preference. 
   Players that have trading scores less than 5 are ignored *)
let should_ask_for cost pl scores : player list =
	let filtered = List.filter (fun p -> (Hashtbl.find scores (color_of p)) > -5 ) (can_ask_for cost pl) in
	List.sort (better_partner scores) filtered

(* return a trade type in order to get resources for building something of buildcost, 
   if such a trade exists *)
let trade_for buildcost history scores inv pl : trade option =
	let need = need_for buildcost inv in
	let can_trade = free_for_trade buildcost inv in
	let num_can_trade = sum_cost can_trade in


	if (one_away buildcost inv) && (num_can_trade > 0) then
		let will_pay =
			if num_can_trade < 2 then can_trade
			else n_resource_cost (highest_resource can_trade) 2 in

		let players_to_ask = should_ask_for buildcost pl scores in
		let have_not_asked p : bool =
			not (List.mem ((color_of p),need,will_pay) history) in

		(* ask a player in pl to give us what we need for what we will pay,
		   as long as we have not asked this already this turn in history *)
		let rec ask pl =
			match pl with
			| [] -> None
			| p::tl when have_not_asked p -> Some((color_of p),need,will_pay)
			| p::tl -> ask tl in
		ask players_to_ask 
	else None



(* takes in a list history that lists the previously requested trades 
   and a hashtable scores that lists the "good trading partner" score 
   for other colors *)
let handle_trade_initiate history scores cm pl : move option =
	let inv = inv_of (player cm pl) in
	let trade buildcost = trade_for buildcost history scores inv pl in
	match trade cCOST_CITY with
	| Some(trd) -> Some (Action(DomesticTrade(trd)))
	| None ->
		match trade cCOST_TOWN with
		| Some(trd) -> Some (Action(DomesticTrade(trd)))
		| None ->
			match trade cCOST_CARD with
			| Some(trd) -> Some (Action(DomesticTrade(trd)))
			| None -> 
				match trade cCOST_ROAD with
					| Some(trd) -> Some (Action(DomesticTrade(trd)))
					| None -> None

let should_initiate_trade history scores cm pl : bool =
	not (is_none (handle_trade_initiate history scores cm pl))


