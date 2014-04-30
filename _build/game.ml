open Definition
open Constant
open Util
open Print


type game = state (* to edit *)

let state_of_game g = g (* to edit *)
let game_of_state s = s (* to edit *)


let init_game () = game_of_state (gen_initial_state())


(* Return information given a player p *)
let color_of (c,h,t) : color = c
let inv_of (c, (i,cds), t) : inventory = i
let cards_of (c, (i,cds), t) : cards = cds
let trophs_of (c, h, t) : trophies = t

(* player c pl return the player corresponding to color c in pl *)
let player (c : color) (pl : player list) : player =
	List.find (fun p -> color_of p = c) pl


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


(* add_road c l rl adds a road on line l for color c to the road list rl.
   Fails if a road already exists on l *)
let add_road c ln rl : road list =
	if List.exists (fun (_,(pt1, pt2)) -> (pt1,pt2) = ln || (pt2,pt1) = ln ) rl
	then failwith "A road already exists on this line."
	else (c, ln)::rl

(* add a town for color c at point pt on intersection list il.
   Raises an exception if there is a previous settlement at that point or
   if the surrounding area is not clear of settlements *)
let add_town c pt il : intersection list =
	let empty_land = pt :: (adjacent_points pt) in
	let _, il' = List.fold_left (fun (ct,l') i ->
		match i with
		| Some(s) when List.mem ct empty_land ->
		    failwith "Cannot place town here. Area is populated by an existing settlement."
		| None when ct = pt -> ( ct+1, Some (c,Town) :: l' )
		| _                 -> ( ct+1, i::l' ))
		(0,[]) il in
	il'


let initial c (pt1,pt2) b : board =
	let m, (il, rl), dk, dis, rob = b in
	let structures' = try ( (add_town c (pt1) il), (add_road c (pt1, pt2) rl) ) 
		              with _ -> failwith "Failed to place initial road and settlement." in
	(m, structures', dk, dis, rob)

let valid_initial c ln b : bool =
	try (fun x -> true) (initial c ln b)
	with _ -> false

let rec random_initialmove c b : move =
	let random_ln = (Random.int 53, Random.int 53) in
	if valid_initial c random_ln b then InitialMove(random_ln)
	else random_initialmove c b


(* list of colors next to a given piece, according to intersection list il *)
let colors_near (pc : piece) (il : intersection list) : color list =
	let adj = piece_corners pc in 
	List.fold_left (fun clist pt -> 
		match List.nth il pt with
		| Some(c,settle) -> c::clist
		| None -> clist )
		[] adj


(* placing the robber on piece pc is a valid move on board b *)
(* robber is not currently at the piece indicated.
   If a color is indicated, it must have a settlement on an adjacent pt.
   If a color is not indicated, ) *)
let valid_rob (pc,copt) b =
	let m, (il, rl), dk, dis, rob = b in
	if pc = rob then false
	else 
		match copt with
		| None when colors_near pc il = []            -> true
		| Some(c) when List.mem c (colors_near pc il) -> true
		| _                                           -> false

let rec random_rob b : move =
	let _,(il,_),_,_,rob = b in
	let ran_pc = Random.int 18 in
	if ran_pc = rob 
		then random_rob b
	else     RobberMove(ran_pc, pick_random (colors_near ran_pc il))


(* if the number of resources in dis is the floor of half the resources that color c owns *)
let valid_discard c dis pl : bool =
	sum_cost dis = (sum_cost (inv_of (player c pl))) / 2

(* subtraction of cost2 from cost1. Returns error if any resources are negative *)
let diff_cost cost1 cost2 : cost =
	map_cost2 (fun a b -> a-b) cost1 cost2

let add_cost cost1 cost2 : cost =
	map_cost2 (fun a b -> a+b) cost1 cost2	

(* c can pay fee *)
let can_pay (p : player ) (fee : cost) =
	try (fun x -> true) (diff_cost (inv_of p) fee)
	with _ -> false

let random_resource () : resource =
	match Random.int 4 with
	| 0 -> Brick
	| 1 -> Wool
	| 2 -> Ore
	| 3 -> Grain
	| 4 -> Lumber
	| _ -> failwith "invalid random number"

(* returns a random cost of n resources to discard from inventory inv *)
let random_discard inv : move =
	(* d is the cost to discard *)
	let rec help_discard n inv d : cost =
		if n = 0 then d
		else
			let ran_rsc = random_resource () in
			(* if player has this random resource, add it to the discard list *)
			if num_resource_in_inventory inv ran_rsc > 0 then  
				let c = single_resource_cost ran_rsc in
				help_discard (n-1) (diff_cost inv c) (add_cost d c)
			else help_discard n inv d in
	DiscardMove(help_discard ((sum_cost inv) / 2) inv (0,0,0,0,0))




(* If c can built a road on line *)
let valid_build_road c ln =
	failwith "riverrun, past Eve and Adam's "
	(* not an existing road: not in board's structure's road list *)
	(* adjacent to a road or town of this player *)
let valid_build_town c pt =
	failwith "riverrun, past Eve and Adam's "
	(* no existing settlement *)
	(* no adjacent settlement. use adjacent_points *)
	(* color has a road at pt *)
let valid_build_city c pt =
	failwith "riverrun, past Eve and Adam's "
	(* c already has a town at pt *)



(* make_valid m rq will return m if m is a valid move in game g. 
   If not, make_valid will return a valid move. *)
let rec make_valid (m : move) (g : game) : move =
	let b, pl, t, (cm, rq) = g in

	match rq, m with
	| InitialRequest, InitialMove(ln)  when valid_initial cm ln b  ->  m
	| InitialRequest, _                                            ->  random_initialmove cm b
	| RobberRequest, RobberMove(rob)   when valid_rob rob b        ->  m 
	| RobberRequest, _                                             ->  random_rob b
	| DiscardRequest, DiscardMove(dis) when valid_discard cm dis pl -> m
	| DiscardRequest, _                                            -> random_discard (inv_of (player cm pl))
	| TradeRequest, TradeResponse(_)                               -> m
	| TradeRequest, _                                              -> TradeResponse(Random.bool())
	| ActionRequest, Action(PlayCard(pc)) when not t.cardplayed    -> m
	| ActionRequest, _ when is_none t.dicerolled                   -> Action(RollDice)
	| ActionRequest, Action(MaritimeTrade(mt)) -> m
		(* when the player has the resources to make the trade w/ num_resources_in_inv; 
		check which ports the player has and their trade ratios *)
	| ActionRequest, Action(DomesticTrade(c, ocost, icost)) -> m
		(* when player has resources ot make the trade and trade limit not reached *) 
	| ActionRequest, Action(BuyBuild(BuildRoad(ln))) when valid_build_road cm ln -> m
		(* and player can pay cost_of_build *)
	| ActionRequest, Action (BuyBuild(BuildTown(pt))) when valid_build_town cm pt -> m
		(* and player can pay *)
	| ActionRequest, Action (BuyBuild(BuildCity(pt))) when valid_build_city cm pt -> m
		(* and player can pay *)
	| ActionRequest, Action (BuyBuild(BuildCard)) -> m
		(* when player can pay *)
	| ActionRequest, _ -> Action(EndTurn) 



let handle_move g m =
	let b, pl, t, (cm, rq) = g in
	let map, (il, rl), dk, dis, rob = b in
	let mv = make_valid m g in

	(* g' is the game after the move has been played *)
	let g' : game = 
		match mv with
		| InitialMove(ln) -> begin
			let b' = initial cm ln b in
			let pnum = List.length(pl) in (* number of players *)
			let inum = List.length(il) in (* number of initilaized towns *)
			let active', n' = 
				if inum <= pnum then (* first stage of initialization *)
					     next_turn cm, (next_turn cm, InitialRequest)
				else let next_color = prev_turn cm in (* second stage *)
					if inum = 2*pnum then 
						 next_color,   (next_color, ActionRequest )
					else next_color,   (next_color, InitialRequest) in
			(b', pl, new_turn active', n')
		end
		| RobberMove (rm) -> failwith "I am the shadow of the waxwing slain"
			(* change board.robber *)
			(* steal resource *)
		| DiscardMove (ns)-> failwith "I am the shadow of the waxwing slain"
			(* use map_cost2 to subtract cost ns from player's resources *)
			(* when the requested player still has more than seven cards. discard the ones they inicated, and discard some more *)
		| TradeResponse (agree) ->
			let t' = { active = t.active ; dicerolled = t.dicerolled ;
			           cardplayed = t.cardplayed ; cardsbought = t.cardsbought ;
			           tradesmade = t.tradesmade ; pendingtrade = None } in
			let n' = t.active, ActionRequest in
			if not agree then (b,pl,t',n')
			else (* make trade *) failwith "I am the shadow of the waxwing slain"	
			(* did NOT change trade counter *)
		| Action (RollDice) -> failwith "I am the shadow of the waxwing slain"
			(* let roll = random_roll () in *)
			(* distribute resources unless a robber is on that tile *)
		| Action (MaritimeTrade(sell, buy)) -> failwith "I am the shadow of the waxwing slain"

		| Action (DomesticTrade(c, outns, inns)) -> failwith "Lolita"
			(* increment trade counter *)
		| Action (BuyBuild(BuildRoad(c,lin))) -> failwith "light of my life, "
		| Action (BuyBuild(BuildTown(pt))) -> failwith "light of my life, "

		| Action (BuyBuild(BuildCity(pt))) -> failwith "light of my life, "
		| Action (BuyBuild(BuildCard)) -> failwith "light of my life, "
			(* remove card from deck *)
		| Action (PlayCard(PlayKnight(rm))) -> failwith "fire of my loins"
			(* add card to discard pile *)
		| Action (PlayCard(PlayRoadBuilding(rd1, Some rd2))) -> failwith "fire of my loins"
		| Action (PlayCard(PlayRoadBuilding(rd1, None))) -> failwith "fire of my loins"		
		| Action (PlayCard(PlayYearOfPlenty(rsc1, Some rsc2))) -> failwith "fire of my loins"
		| Action (PlayCard(PlayYearOfPlenty(rsc1, None))) -> failwith "fire of my loins"
		| Action (PlayCard(PlayMonopoly(rsc))) -> failwith "fire of my loins"
		| Action (EndTurn) ->
			(* distribute cards that have been bought *)
			let pl' = add_cards t.cardsbought t.active pl in
			(* reset turn information, ask next player for action *)
			let next_active = next_turn t.active in
			let t' = new_turn next_active in
			(b, pl', t', (next_active, ActionRequest))
	in
	let winner : color option =
		(* if various win conditions -> some color , else *)
		None in
	(winner, g')


let presentation g = g (* to edit *)
(* hide cards of all other players with util.hide *)
(* hide cardsbought by active player *)
