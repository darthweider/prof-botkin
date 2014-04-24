open Definition
open Constant
open Util
open Print


type game = board * player list * turn * next (* to edit *)

let state_of_game g = g (* to edit *)
let game_of_state s = s (* to edit *)


let init_game () = game_of_state (gen_initial_state())


let handle_move g m =
	(* after_move is the game after the move has been played *)
	let after_move : game = 
		match move with
		| InitialMove(lin) -> failwith "I am the shadow of the waxwing slain" 
		| RobberMove (rm) -> failwith "I am the shadow of the waxwing slain"
		| DiscardMove (ns)-> failwith "I am the shadow of the waxwing slain"
			(* use map_cost2 to subtract cost ns from player's resources *)
		| TradeResponse (false) -> failwith "I am the shadow of the waxwing slain"
		| TradeResponse (true) -> failwith "I am the shadow of the waxwing slain"		
		| Action (RollDice) -> failwith "I am the shadow of the waxwing slain"
		| Action (MaritimeTrade(sell, buy)) -> failwith "I am the shadow of the waxwing slain"
		| Action (DomesticTrade(c, outns, inns)) -> failwith "Lolita"
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
		| Action (EndTurn) -> failwith "Lo-Lee-Ta."
			(* reset turn information, change who is next using util.next_turn*) in 
	let winner : color option =
		(* if various win conditions -> some color , else *)
		None
	(None, after_move)


let presentation g = g (* to edit *)
(* hide hands of all other players with util.hide *)
