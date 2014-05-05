open Definition
open Constant
open Util
open Print
open Printf
open Player (* functions for handling assets and players *)
open Structures (* functions for roads and setlements: building and initializing *)
open Trophies

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
	let (hlist, plist), (il, rl), dk, dis, rob = b in


	match rq, m with
	| InitialRequest, InitialMove(ln)  when valid_initial cm ln b   ->  m
	| InitialRequest, _                                             ->  random_initialmove cm b
	| RobberRequest, RobberMove(rob)   when valid_rob rob b         ->  m 
	| RobberRequest, _                                              ->  random_rob b
	| DiscardRequest, DiscardMove(dis) when valid_discard cm dis pl ->  m
	| DiscardRequest, _                                             ->  random_discard (inv_of (player cm pl))
	| TradeRequest, TradeResponse(_)                                ->  m
	| TradeRequest, _                                               ->  TradeResponse(Random.bool())
	| ActionRequest, Action(PlayCard(PlayKnight(rbm))) when not t.cardplayed && (valid_knight cm pl) && valid_rob rbm b  ->  m
	| ActionRequest, Action(PlayCard(PlayRoadBuilding(rd1, rd2))) when not t.cardplayed       ->  m
	| ActionRequest, Action(PlayCard(PlayYearOfPlenty(res1, res2))) when not t.cardplayed     ->  m
	| ActionRequest, Action(PlayCard(PlayMonopoly(res))) when not t.cardplayed && (valid_monopoly cm pl)               ->  m
	| ActionRequest, _ when is_none t.dicerolled                                              ->  Action(RollDice)
	| ActionRequest, Action(MaritimeTrade(r1, _)) when valid_mari_trade cm pl il plist r1     -> m
		(* when the player has the resources to make the trade w/ num_resources_in_inv; 
		check which ports the player has and their trade ratios *)
	| ActionRequest, Action(DomesticTrade(c, ocost, icost)) when valid_dom_trade cm c ocost icost pl t  -> m
		(* when player has resources ot make the trade and trade limit not reached *) 
	| ActionRequest, Action(BuyBuild(BuildRoad(rd))) when valid_build_road cm pl rd  rl                 -> m
		(* and player can pay cost_of_build *)
	| ActionRequest, Action (BuyBuild(BuildTown(pt))) when valid_build_town cm pt pl rl il              -> m
		(* and player can pay *)
	| ActionRequest, Action (BuyBuild(BuildCity(pt))) when valid_build_city cm pt pl il                 -> m
		(* and player can pay *)
	| ActionRequest, Action (BuyBuild(BuildCard)) when valid_build_card cm pl dk                        -> m
		(* when player can pay *)
	| ActionRequest, _                                                                                  -> Action(EndTurn) 



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
			(*Discard, then check if any other players must discard. If not, have the inital player move the robber*)
			let pl' = rm_from_inv ns cm pl in
			next_to_discard cm pl' t b
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

			if roll = cROBBER_ROLL then
				(*Everyone discard if over 7 resources. Move the robber after discarding*)
				next_to_discard t.active pl t' b
			(* Else distribute resources *)
			else let pl' = 
					List.fold_left (fun placc (pc, hex) ->
						let t, n = hex in
						if (pc != rob && n = roll && t != Desert) 
						then distribute_to_pts (piece_corners pc) (get_some (resource_of_terrain t)) il placc
						else placc )
						pl (indexed hexl) in

				let n' = (cm, ActionRequest) in
				(b, pl', t', n')


		| Action (MaritimeTrade(sell, buy)) ->begin
			let ports = get_ports cm il portl in
			let rate = best_trade_rate ports sell in
			let cost = n_resource_cost sell rate in
			let reward = n_resource_cost buy 1 in
			(*Remove minimum amount of resources possible*)
			let pl' = rm_from_inv cost cm pl in
			(*Receive one resource back*)
			let pl' = add_to_inv reward cm pl' in
			(*Trade is done*)
			let n' = (cm, ActionRequest) in
			(b, pl', t, n')
		end

		| Action (DomesticTrade(c, outns, inns)) -> begin
			let t' = { active = t.active ; dicerolled = t.dicerolled ; cardplayed = t.cardplayed ;
			          cardsbought = t.cardsbought ; tradesmade = t.tradesmade+1 ; 
			          pendingtrade = Some(c, outns, inns) } in
			(*Verify that the trade is legal--enough recourses must exist*)
			(b, pl, t', (c, TradeRequest))
			
		end


		(*Subtract fee. Place Road. ActionRequest*)
		| Action (BuyBuild(BuildRoad(c,lin))) ->
			(*Pay up*)
			let pl' = rm_from_inv cCOST_ROAD c pl in
			(*Place road*)
			let rl' = add_road (c,lin) rl in
			let b' = (hexl, portl), (il, rl'), dk, dis, rob in
			(b', pl', t, (cm, ActionRequest))

		| Action (BuyBuild(BuildTown(pt))) ->
			(*Pay up*)
			let pl' = rm_from_inv cCOST_TOWN cm pl in
			(*Place town--MIGHT want to change this to a function that can't fail. At this point the town should be valid, but better safe than sorry*)
			let il' = add_town cm pt il in
			let b' = (hexl, portl), (il', rl), dk, dis, rob in
			(b', pl', t, (cm, ActionRequest))

		| Action (BuyBuild(BuildCity(pt))) -> 
			(*Pay up*)
			let pl' = rm_from_inv cCOST_CITY cm pl in
			(*Place city. This is done by removing the town and replacing it with a city*)
			let il' = add_city cm pt il in
			let b' = (hexl, portl), (il', rl), dk, dis, rob in
			(b', pl', t, (cm, ActionRequest))
			
		| Action (BuyBuild(BuildCard)) ->
			(*Pay up*)
			let pl' = rm_from_inv cCOST_CARD cm pl in
			(*Unwrap the deck*)
			let revdeck = reveal dk in
			(*Get the deck size*)
			let dksize = List.length revdeck in
			(*Draw a random card from the range [0...dksize-1]*)
			let rmcard_index = Random.int dksize in
			let card = List.nth (reveal dk) rmcard_index in
			(*Remove the card from the unwrapped list; rewrap the new deck*)
			let dk' = Reveal (List.filter (fun x -> x <> card) revdeck) in
			(*Store the drawn card*)
			let cardsb = Reveal( card::(reveal t.cardsbought)) in
			let t' = { active = t.active ; dicerolled = t.dicerolled ; cardplayed = t.cardplayed ;
			          cardsbought = cardsb ; tradesmade = t.tradesmade ; 
			          pendingtrade = t.pendingtrade} in
			          (*Update the board with new deck*)
			let b' = (hexl, portl), (il, rl), dk', dis, rob in
			(*Update the game with new board, new playerlist (costs removed), new turn (card added)*)
			(b', pl', t', (cm, ActionRequest))
		| Action (PlayCard(PlayKnight(pc, copt))) ->

			let p = player cm pl in
			(*our hand*)
			let h = reveal (cards_of p) in
			(*Note that we played a card*)
			let t' = { active = t.active ; dicerolled = t.dicerolled ; cardplayed = true;
			          cardsbought = t.cardsbought ; tradesmade = t.tradesmade ; 
			          pendingtrade = t.pendingtrade} in
			(*remove the card from our hand*)
			let (_, h') = have_card_of Knight h in
			(*add the card to discard*)
			let dis' = Knight::dis in
			(*Move the robber*)
			let b' = (hexl, portl), (il, rl), dk, dis', pc in

			let pl'' =
				match copt with
				| None -> pl
				| Some(c) -> begin
					(* the cost of one random resource that c has *)
					let stolen = n_random_resources (inv_of (player c pl)) 1 in
					let pl' = rm_from_inv stolen c pl in
					add_to_inv stolen cm pl' 
				end in
			let pl'' = update cm pl'' (fun (c, (inv, han), (kn, t1, t2)) -> (c, (inv, Reveal(h')), (kn+1, t1, t2))) in
			(*Update largest army*)
			let n' = cm, ActionRequest in
			(b', pl'', t', n')

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
		if has_won t.active il pl then Some t.active
		else None in
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