open Definition
open Constant
open Util
open Print
open Printf

(*======================INFORMATION RETRIEVAL=================================*)

(* Return information given a player p *)
let color_of (c,h,t) : color = c
let inv_of (c, (i,cds), t) : inventory = i
let cards_of (c, (i,cds), t) : cards = cds
let trophs_of (c, h, t) : trophies = t
let num_knights p : int = let (k,_,_) = trophs_of p in k
let has_longest_road p : bool = let (_,longest,_) = trophs_of p in longest
let has_largest_army p : bool = let (_,_,largest) = trophs_of p in largest
let num_res_of (c, (i, cds), t) res = num_resource_in_inventory i res

(* player c pl return the player corresponding to color c in pl *)
let player (c : color) (pl : player list) : player =
	List.find (fun p -> color_of p = c) pl


(*==============================COST/RESOURCE CALCULATIONS====================================*)

let empty_cost = (0,0,0,0,0)

(* subtraction of cost2 from cost1. The return value can be negative *)
let diff_cost cost1 cost2 : cost =
map_cost2 (fun a b -> 
		a-b) cost1 cost2

	(* Note from Matthew: I've commented out your code here. I don't think we want it to return an error.
	map_cost2 (fun a b -> 
		let diff = a-b in
		if diff > 0 then diff
		else failwith "cannot have a negative cost") cost1 cost2 *)

(* subtraction of cost2 from cost1. If the difference is negative for any resource, make it 0. *)
let floor_diff_cost cost1 cost2 : cost =
	map_cost2 (fun a b ->
		max 0 (a-b) ) cost1 cost2

(* addition of two costs *)
let add_cost cost1 cost2 : cost =
	map_cost2 (fun a b -> a+b) cost1 cost2	

(* c can pay fee *)
let can_pay (p : player ) (fee : cost) =
	let inv' = diff_cost (inv_of p) fee in
	let check_for_negatives ((b,w,o,l,g) : cost) : bool =
		b>=0 && w>=0 && o>=0 && l>=0 && g>=0 in
	check_for_negatives inv'

(* n_of r n returns a cost for n of resource r *)
let n_resource_cost resource n : cost =
  match resource with
    | Brick ->  (n,0,0,0,0)
    | Wool ->   (0,n,0,0,0)
    | Ore ->    (0,0,n,0,0)
    | Grain ->  (0,0,0,n,0)
    | Lumber -> (0,0,0,0,n)

let random_resource () : resource =
	match Random.int 5 with
	| 0 -> Brick
	| 1 -> Wool
	| 2 -> Ore
	| 3 -> Grain
	| _ -> Lumber

(* from an inventory inv, choose n random resources and return the cost of all those resources *)
let n_random_resources inv n : cost =
	if sum_cost inv = 0 then empty_cost
	else let rec helper inv n chosen : cost =
		if n = 0 then chosen
		else
			let ran_rsc = random_resource () in
			(* if player has this random resource, add it to the chosen list *)
			if num_resource_in_inventory inv ran_rsc > 0 then  
				let c = single_resource_cost ran_rsc in
				helper (diff_cost inv c) (n-1) (add_cost chosen c)
			else let () = print_int n; print_string "n_random_resources" in
			helper inv n chosen in
	helper inv n empty_cost


(*=======================UPDATE PLAYER INFORMATION=====================*)


(* update information of color c in pl with f.
   f takes a player and returns a player *)
let update (c : color) (pl : player list) f : player list =
	List.map ( fun p -> 
		if c = color_of p then f p 
		else p ) pl

(* give add cards cds to player of color c; returns the updated player list *)
let add_cards new_cds c pl : player list =
	update c pl (fun p -> 
		let added = List.fold_left (fun acc_cards new_cd -> append_card acc_cards new_cd) 
			(cards_of p) (reveal new_cds) in
		(c, (inv_of p, added ), trophs_of p))

(* add cost income to the inventory of color c; returns updated player list *)
let add_to_inv income c pl : player list =
	update c pl (fun p ->
		let inv' = add_cost (inv_of p) income in
		(c, (inv', cards_of p), trophs_of p))

let rm_from_inv expense c pl : player list =
	update c pl (fun p ->
		let inv' = diff_cost (inv_of p) expense in
		(c, (inv', cards_of p), trophs_of p))


(* if the number of resources in dis is the floor of half the resources that color c owns 
   AND the number of resources originally owned by c is greater than 7 *)
let valid_discard c dis pl : bool =
	let p = player c pl in
	(sum_cost (inv_of (player c pl)))  > 7 && sum_cost dis = (sum_cost (inv_of (player c pl))) / 2 && can_pay p dis


(* returns a random discard move that will discard random resources 
   so that the resulting inv would be the floor of half the current inventory *)
let random_discard inv : move =
	if sum_cost inv > 7 then DiscardMove(n_random_resources inv ((sum_cost inv) / 2) )
	else DiscardMove(0,0,0,0,0)



(* distribute resource to the color at point pt, based on intersection list il.
   Update player list pl accordingly. *)
let distribute resource pt il pl : player list =
		match List.nth il pt with
		| Some(c,s) -> 
			print ("distributing " ^ (string_of_resource resource) ^ (sprintf " to pt %i" pt)) ;
			add_to_inv (n_resource_cost resource (settlement_num_resources s)) c pl
		| _ -> pl

(* distribute resource to all points in list pts, based on intersection list il.
   Update player list pl accordingly. *)
let distribute_to_pts pts resource il pl : player list =
	List.fold_left ( fun placc pt -> 
		distribute resource pt il placc ) pl pts 

(* Given a list of pieces, return a list of resources that are generated from the
   pieces given *)
let resources_from pcs hexl : resource list =
	List.fold_left ( fun resrc_acc pc -> 
		let (t,n) = List.nth hexl pc in
		match resource_of_terrain t with
		| None -> resrc_acc
		| Some(r) -> r::resrc_acc ) [] pcs

(* distributes initial resources. 
   Given a point (presumably the second chosen initial town), 
   distribute to the color at that point one of each resource from the surrounding hexes.
   Updates the player list accordingly. *)
let distribute_initial pt b pl : player list =
	let (hexl,_),(il,_),_,_,_ = b in
	print (sprintf "distribute_initial for %i" pt) ; 
	List.fold_left ( fun placc resource -> 
		distribute resource pt il placc ) 
		pl (resources_from (adjacent_pieces pt) hexl)


let valid_dom_trade (pinit :color) (ptarget:color) (ocost :cost ) (icost : cost) (pl : player list) (t: turn) : bool = 
	(can_pay (player pinit pl) ocost) && (can_pay (player ptarget pl) icost) && t.tradesmade < cNUM_TRADES_PER_TURN

let rec next_to_discard (cm : color) (pl : player list) (t' : turn) (b: board) : state =
	let pnext = next_turn cm in
	if (sum_cost (inv_of (player pnext pl)))  > cMAX_HAND_SIZE then (b, pl, t', ((pnext), DiscardRequest))
	else begin
		if pnext = t'.active then (b, pl, t', ((pnext), RobberRequest))
		else next_to_discard pnext pl t' b
	end

(* Given a color c and an intersection list il, get_ports returns a list of ports owned by this player
	The elements of this list are of the form (exchange rate, type of resource) *)
let get_ports (c : color) (il : intersection list) (port_list : port list) : (int * portresource) list=
	let rec helper port_list acc = 
		match port_list with
			| [] -> acc
			| ((loc, loc'), rate, res_type) ::tl ->
			let cons_if_not_none l x =
				match x with 
				| None -> l
				| Some(col, _) -> if c = col then (rate, res_type)::l
								  else l in
			let port_one =  List.nth il loc in
			let port_two =  List.nth il loc' in 
			let acc = cons_if_not_none acc port_one in
			let acc = cons_if_not_none acc port_two in
			helper tl acc in
	helper port_list []

let best_trade_rate (port_list : (int * portresource) list) (res : resource) : int =
	let rec helper l best_rate =
		match l with
		| [] -> best_rate
		| (r, Any)::tl -> helper tl (min r best_rate)
		| (r, PortResource(rs))::tl -> if rs = res then helper tl (min r best_rate)
									   else helper tl best_rate in
	helper port_list cMARITIME_DEFAULT_RATIO

let valid_mari_trade (c : color) (pl : player list) (il : intersection list) (plist : port list) (res : resource) : bool = 
	let ports = get_ports c il plist in
	let rate = best_trade_rate ports res in
	let p = player c pl in
	let cost = n_resource_cost res rate in
	can_pay p cost



let valid_build_card (c: color) (pl : player list) (d: deck) : bool =
	(*Verify that player has enough resources
	  Verify that the deck is not empty*)
	let p = player c pl in
	let decksize = match d with 
		| Hidden(x) -> x
		| Reveal(l) -> List.length l in
	can_pay p cCOST_CARD  && decksize > 0

(*Returns a tuple of (bool, card list) where bool is (this card is in our hand) and the card list is the hand with one of card
	(if any) removed*)
let have_card_of (cd : card) (h : card list) : bool * (card list) =
	let rec helper h acc flag = 
	match h with
		| c::tl -> if flag then begin
			if c = cd then helper tl acc false
			else helper tl (c::acc) flag
		end
		else helper tl (c::acc) flag
		| [] -> ((not flag), acc) in
	helper h [] true

let valid_play_card card c pl : bool =
	let p = player c pl in
	let h' = reveal (cards_of p) in
	fst (have_card_of card h')	

let valid_knight (c : color) (pl : player list) : bool =
	(*Player has the card in hand*)
	valid_play_card Knight c pl
	
let valid_monopoly (c : color) (pl : player list) (res : resource) : bool =
	let p = player c pl in
	let cost = n_resource_cost res 1 in
	(*Player has at least one of said resource AND has a monopoly card in their hand*)
	can_pay p cost && valid_play_card Monopoly c pl

let valid_year (c : color) (pl : player list) : bool =
	(*Player has the card in hand*)
	valid_play_card YearOfPlenty c pl


	




(* NOT IN USE. PROBABLY BROKEN.
   collects the resources generated by the structure at intersection i on point pt
   when roll is rolled. Add these resources to the appropriate place in player list pl  *)
(* let collect roll i pt b pl : player list =
	let (hexl,_),_,_,_,rob = b in

	match i with
	| None -> pl
	| Some((c,s)) -> 
		(* list of adjacent pieces that might generate resources 
		   does not include an adjacent piece that a robber is on *)
		let adj_pieces = List.filter (fun x -> x != rob) (adjacent_pieces pt) in
		(* list of resources that can be collected for the settlement at this intersection
		   resources may be duplicated, indicating duplicate collection *)
		let resources_to_collect = resources_from adj_pieces roll hexl in
		(* conversion of the resources to collect into a cost type
		   taking into account the difference in collection amt for different settlement types *)
		let cost_to_collect = List.fold_left (fun cost_acc resrc -> 
			add_cost cost_acc (n_resource_cost resrc (settlement_num_resources s)) )
			empty_cost resources_to_collect in
		(* return an updated player list with the cost added to the inventory of 
		   the color at this intersection *)
		add_inv cost_to_collect c pl
*)