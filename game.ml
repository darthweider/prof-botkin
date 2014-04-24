open Definition
open Constant
open Util
open Print


type game = board * player list * turn * next (* to edit *)

let state_of_game g = g (* to edit *)
let game_of_state s = s (* to edit *)


let init_game () = game_of_state (gen_initial_state())


(* make_valid m rq will return m if m is a valid move in response to 
   request rq. If not, make_valid will return a valid move. *)
let make_valid (m : move) (rq : request) : move =
	failwith "riverrun, past Eve and Adam's, "

(*(* given a color, return the corresponding player *)
let player (c : color) (pl : player list) : player =
	failwith "riverrun, past Eve and Adam's, "*)

(* update information of color c in pl with f.
   f takes in a hand and a trophies, and returns a player *)
let update (c : color) (pl : player list) f : player list =
	List.map ( fun (c',h',tr') -> 
		if c = c' then f h' tr' 
		else (c',h',tr') ) pl
(*(* update inventory of c in pl with i *)
let update_inv (c : color) (i : inventory) (pl : player list) : player list =
	update c pl (fun (i',cs') tr' -> (c, (i,h'), tr'))
(* update cards of c in pl with cs *)
let update_cards  (c : color) (cs : cards) (pl : player list) : player list =
	update c pl (fun (i',cs') tr' -> (c, (i',cs),tr'))
(* update trophies of c in pl with trs *)
let update_trophies (c : color) (tr : trophies) (pl : player list) : player list =
	update c pl (fun (i',cs') tr' -> (c, (i',cs'), tr))*)

(* give add cards cds to player of color c; returns the updated player list *)
let add_cards cds c pl : player list =
	update c pl (fun (i',cds') tr' -> 
		added = List.map ( fun cd -> append_card cds' cd ) (reveal cds)


let handle_move g m =
	let (b,pl, t, (rqc, rq)) = g in
	let mv = make_valid m rq in

	(* g' is the game after the move has been played *)
	let g' : game = 
		match mv with
		| InitialMove(lin) -> failwith "I am the shadow of the waxwing slain" 
		| RobberMove (rm) -> failwith "I am the shadow of the waxwing slain"
		| DiscardMove (ns)-> failwith "I am the shadow of the waxwing slain"
			(* use map_cost2 to subtract cost ns from player's resources *)
		| TradeResponse (agree) ->
			let t' = { active = t.active ; dicerolled = t.dicerolled ;
			           cardplayed = t.cardplayed ; cardsbought = t.cardsbought ;
			           tradesmade = t.tradesmade ; pendingtrade = None }
			let n' = t.active, ActionRequest in
			if not agree then (b,pl,t',n')
			else failwith "I am the shadow of the waxwing slain"	
			(* did NOT change trade counter *)
		| Action (RollDice) -> failwith "I am the shadow of the waxwing slain"
		| Action (MaritimeTrade(sell, buy)) -> failwith "I am the shadow of the waxwing slain"
		| Action (DomesticTrade(c, outns, inns)) -> failwith "Lolita"
			(* increment trade counter *)
		| Action (BuyBuild(BuildRoad(c,lin))) -> failwith "light of my life, "
		| Action (BuyBuild(BuildTown(pt))) -> failwith "light of my life, "
		| Action (BuyBuild(BuildCity(pt))) -> failwith "light of my life, "
		| Action (BuyBuild(BuildCard)) -> failwith "light of my life, "
		| Action (PlayCard(PlayKnight(rm))) -> failwith "fire of my loins"
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
