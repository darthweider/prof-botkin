open Definition
open Constant
open Util
open Print
open Player (* functions for handling assets and players *)
open Structures (* functions for roads and setlements: building and initializing *)

type game = state (* to edit *)

let state_of_game g = g (* to edit *)
let game_of_state s = s (* to edit *)


let init_game () = game_of_state (gen_initial_state())




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





(* make_valid m rq will return m if m is a valid move in game g. 
   If not, make_valid will return a valid move. *)
let rec make_valid (m : move) (g : game) : move =
	let b, pl, t, (cm, rq) = g in

	match rq, m with
	| InitialRequest, InitialMove(ln)  when valid_initial cm ln b   ->  m
	| InitialRequest, _                                             ->  random_initialmove cm b
	| RobberRequest, RobberMove(rob)   when valid_rob rob b         ->  m 
	| RobberRequest, _                                              ->  random_rob b
	| DiscardRequest, DiscardMove(dis) when valid_discard cm dis pl ->  m
	| DiscardRequest, _                                             ->  random_discard (inv_of (player cm pl))
	| TradeRequest, TradeResponse(_)                                ->  m
	| TradeRequest, _                                               ->  TradeResponse(Random.bool())
	| ActionRequest, Action(PlayCard(pc)) when not t.cardplayed     ->  m
	| ActionRequest, _ when is_none t.dicerolled                    ->  Action(RollDice)
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
	let (hexl, portl), (il, rl), dk, dis, rob = b in
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
		| Action (RollDice) ->
			let roll = random_roll () in

			let t' = { active = t.active ; dicerolled = Some(roll) ; cardplayed = t.cardplayed ;
			          cardsbought = t.cardsbought ; tradesmade = t.tradesmade ; 
			          pendingtrade = t.pendingtrade } in

			if roll = cROBBER_ROLL then (b, pl, t', (cm, RobberRequest))

			(* distribute resources *)
			else 
				let _, pl' = 
					(* go through each intersection and collect any resources generated by 
				       a settlement at that intersection *)
					List.fold_left ( fun (ct,placc) i -> 
						(ct-1), (collect roll i ct b placc) ) 
						(List.length(il)-1, pl) il in

				let n' = (cm, ActionRequest) in
				(b, pl', t', n')


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


let presentation g : game =
	let b, pl, t, (cm,rq) = g in
	let m, s, dk, dis, rob = b in

	(* hide deck if necessary *)
	let dk' = hide dk in
	(* hide cards of all other players *)
	let pl' = List.map ( fun p -> if color_of p = cm then p 
		                          else let (c, (i, cds), tr) = p in (c, (i, hide cds), tr))  pl in
	(* hide cardsbought by active player *)
	let t' = { active = t.active ; dicerolled = t.dicerolled ; cardplayed = t.cardplayed ;
	           cardsbought = hide t.cardsbought ; tradesmade = t.tradesmade ; 
	           pendingtrade = t.pendingtrade } in

	((m, s, dk', dis, rob), pl', t', (cm, rq))