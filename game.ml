open Definition
open Constant
open Util
open Print


type game = state (* to edit *)

let state_of_game g = g (* to edit *)
let game_of_state s = s (* to edit *)


let init_game () = game_of_state (gen_initial_state())


(* If c can built a road on line *)
let valid_road c ln =
	failwith "riverrun, past Eve and Adam's "
	(* not an existing road: not in board's structure's road list *)
	(* adjacent to a road or town of this player *)
let valid_town c pt =
	failwith "riverrun, past Eve and Adam's "
	(* no existing settlement *)
	(* no adjacent settlement. use adjacent_points *)
	(* color has a road at pt *)
let valid_city c pt =
	failwith "riverrun, past Eve and Adam's "
	(* c already has a town at pt *)
let valid_initial ln =
	failwith "riverrun, past Eve and Adam's "
	(* not an existing road *)
	(* no adjacent settlement next to p1. use adjacent_points *)

(* subtraction of cost2 from cost1. Returns error if any resources are negative *)
let sub_cost cost1 cost2 =
	failwith "Many years later, as he faced the firing squad, "	

(* c can pay fee *)
let can_pay (c : color ) (fee : cost) =
	failwith "Many years later, as he faced the firing squad, "


(* make_valid m rq will return m if m is a valid move in response to 
   request rq. If not, make_valid will return a valid move. *)
let rec make_valid (m : move) (c : color) (rq : request) (g : game) : move =
	match rq, m with
	| InitialRequest, InitialMove(ln) when valid_initial ln -> m
	| InitialRequest, _ -> 
		make_valid (InitialMove(Random.int 53, Random.int 53)) c rq g
	| RobberRequest, RobberMove(rob) -> m (* when board.robber is not the piece indicated, and the color indicated is on an adjacent pt *)
	| RobberRequest, _ -> 
		make_valid (RobberMove(Random.int 18, random_color())) c rq g
	| DiscardRequest, DiscardMove(cost) -> (* when the requested player has < 7 cards *)
	| DiscardRequest, DiscardMove(cost) -> (* when the requested player still has more than seven cards. discard the ones they inicated, and discard some more *)
	| DiscardRequest, _ -> (* discard however many cards to get to seven cards *)
	| TradeRequest, TradeResponse(r) -> m
	| TradeRequest, _ -> (* randomly choose T or F *)
	| ActionRequest, _ (* when dice not rolled yet *) -> Action(RollDice)
	| ActionRequest, Action(MaritimeTrade(mt)) (* when the player has the resources to make the trade; check which ports the player has and their trade ratios *)-> m
	| ActionRequest, Action(DomesticTrade(c, ocost, icost)) (* when player has resources ot make the trade and trade limit not reached *) -> m 
	| ActionRequest, Action(BuyBuild(BuildRoad(r))) when valid_road c ln (* and player can pay *)-> m
	| ActionRequest, Action (BuyBuild(BuildTown(pt))) when valid_town (* and player can pay *) -> m
	| ActionRequest, Action (BuyBuild(BuildCity(pt))) when valid_city (* and player can pay *)-> m
	| ActionRequest, Action (BuyBuild(BuildCard)) (* when player can pay *)-> m
	| ActionRequest, _ -> Action(EndTurn) 

(*(* given a color, return the corresponding player *)
let player (c : color) (pl : player list) : player =
	failwith "riverrun, past Eve and Adam's, "*)


(* color_of p returns the color of player p *)
let color_of (c,h,t) : color = c
let inv_of (c, (i,cds), t) : inventory = i
let cards_of (c, (i,cds), t) : card list = cds
let trophs_of (c, h, t) : trophies = t

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



let handle_move g m =
	let (b,pl, t, (rqc, rq)) = g in
	let mv = make_valid m rq in

	(* g' is the game after the move has been played *)
	let g' : game = 
		match mv with
		| InitialMove(pt1,pt2) -> failwith "I am the shadow of the waxwing slain" 
			(* change intersection list and road list *)
		| RobberMove (rm) -> failwith "I am the shadow of the waxwing slain"
			(* change board.robber *)
			(* steal resource *)
		| DiscardMove (ns)-> failwith "I am the shadow of the waxwing slain"
			(* use map_cost2 to subtract cost ns from player's resources *)
		| TradeResponse (agree) ->
			let t' = { active = t.active ; dicerolled = t.dicerolled ;
			           cardplayed = t.cardplayed ; cardsbought = t.cardsbought ;
			           tradesmade = t.tradesmade ; pendingtrade = None }
			let n' = t.active, ActionRequest in
			if not agree then (b,pl,t',n')
			else (* make trade *)failwith "I am the shadow of the waxwing slain"	
			(* did NOT change trade counter *)
		| Action (RollDice) -> failwith "I am the shadow of the waxwing slain"
			random_roll ()
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
			let next_active = next_turn t.active
			let t' = new_turn next_active in
			(b, pl', t', (next_active, ActionRequest))

	let winner : color option =
		(* if various win conditions -> some color , else *)
		None
	(None, g');
	update_print


let presentation g = g (* to edit *)
(* hide cards of all other players with util.hide *)
(* hide cardsbought by active player *)
