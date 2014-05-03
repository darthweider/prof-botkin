open Definition
open Constant
open Util
open Print
open Printf
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
   If a color is not indicated, there must be no colors adjacent to pc. *)
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
		| InitialMove(pt1,pt2) -> begin
			let b' = initial cm (pt1,pt2) b in
			let pnum = List.length(pl) in (* number of players *)
			let townnum = num_towns_total pl il in (* number of initilaized towns *)
			let pl', active', n' = 
				if townnum < pnum-1 then (* first stage of initialization *)
					pl, next_turn cm, (next_turn cm, InitialRequest)
				else if townnum = pnum-1 then (* go again *)
					pl, cm, (cm, InitialRequest)
				else if townnum < 2*pnum-1 then (* second stage *)
					(distribute_initial pt1 b' pl), 
					prev_turn cm, (prev_turn cm, InitialRequest) 
				else (* last initialization *) 
					(distribute_initial pt1 b' pl), 
					cm, (cm, ActionRequest ) in
			(b', pl', new_turn active', n')
		end
		| RobberMove (pc, copt) -> begin
			let b' = (hexl, portl), (il, rl), dk, dis, pc in

			let pl'' =
				match copt with
				| None -> pl
				| Some(c) -> begin
					(* the cost of one random resource that c has *)
					let stolen = n_random_resources (inv_of (player c pl)) 1 in
					let pl' = rm_from_inv stolen c pl in
					add_to_inv stolen cm pl' 
				end in
			
			let n' = cm, ActionRequest in

			(b', pl'', t, n')
		end
		| DiscardMove (ns)-> begin
			let pl' = rm_from_inv ns cm pl in
			(*If the player discarding is the player who rolled the robber, have them move the robber as their next move*)
			if cm = t.active then (b, pl', t, (cm, RobberRequest))
			(*Otherwise continue removing resources*)
			else (b, pl', t, ((next_turn cm), DiscardRequest))

		end

		| TradeResponse (agree) -> begin
			let t' = { active = t.active ; dicerolled = t.dicerolled ;
			           cardplayed = t.cardplayed ; cardsbought = t.cardsbought ;
			           tradesmade = t.tradesmade ; pendingtrade = None } in
			let n' = t.active, ActionRequest in
			match t.pendingtrade with
			| Some(c, outns, inns) -> begin				
				if not agree then (b,pl,t',n')
				else begin
					let t' = { active = t.active ; dicerolled = t.dicerolled ;
				    	       cardplayed = t.cardplayed ; cardsbought = t.cardsbought ;
				        	   tradesmade = t.tradesmade+1 ; pendingtrade = None } in
				    (*Remove proposing player's resources*)
				    let pl' = rm_from_inv outns t.active pl in
				    (*Remove receiving player's resources*)
				    let pl' = rm_from_inv inns c pl' in
				    (*Give proposing player their resources*)
				    let pl' = add_to_inv inns t.active pl' in
				    (*Give receiving player their resources*)
				    let pl' = add_to_inv outns c pl' in	
				    (*Trade is done*)
				    (b, pl', t', n')
				end
			end
			| _ -> (b,pl,t',n')
		end
		| Action (RollDice) ->
			let roll = random_roll () in

			let t' = { active = t.active ; dicerolled = Some(roll) ; cardplayed = t.cardplayed ;
			          cardsbought = t.cardsbought ; tradesmade = t.tradesmade ; 
			          pendingtrade = t.pendingtrade } in

			if roll = cROBBER_ROLL then (b, pl, t', ((next_turn cm), DiscardRequest))
			(* ==================TO EDIT: DISCARD IF OVER 7 CARDS==================*)


			(* distribute resources *)
			else let pl' = 
					List.fold_left (fun placc (pc, hex) ->
						let t, n = hex in
						if (pc != rob && n = roll && t != Desert) 
						then distribute_to_pts (piece_corners pc) (get_some (resource_of_terrain t)) il placc
						else placc )
						pl (indexed hexl) in

				let n' = (cm, ActionRequest) in
				(b, pl', t', n')


		| Action (MaritimeTrade(sell, buy)) -> failwith "I am the shadow of the waxwing slain"

		| Action (DomesticTrade(c, outns, inns)) -> begin
			let t' = { active = t.active ; dicerolled = t.dicerolled ; cardplayed = t.cardplayed ;
			          cardsbought = t.cardsbought ; tradesmade = t.tradesmade ; 
			          pendingtrade = Some(c, outns, inns) } in
			(*Verify that the trade is legal--enough recourses must exist*)
			if (can_pay (player cm pl) outns) && (can_pay (player c pl) inns) then (b, pl, t', (c, TradeRequest))
			else (b, pl, t, (cm, ActionRequest))
		end
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
	print_update cm mv (state_of_game g');
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