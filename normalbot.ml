open Definition
open Registry
open Constant
open Util
open Structures
open Roadbfs
open Bot_settlements2
open Bot_resources
open Player
open Robber
open Bot_cards
open Print

(** Give your bot a 2-20 character name. *)
let name = "normalbot"


module Bot = functor (S : Soul) -> struct
  (*Our refs*)
  let history = ref([])
  let scores = Hashtbl.create 4
  let prev_trade = ref((0,0,0,0,0),None)

  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () =
    history:= [];
    Hashtbl.clear scores;
    (List.iter (fun col -> Hashtbl.add scores col 0) cDEFAULT_COLORS);
    prev_trade:=(0,0,0,0,0),None


  (*Reset any turn-based refs*)
  let next_turn () = 
    history:= [];
    (*Tuple of inventory, and (some player) we traded with*)
    prev_trade:=(0,0,0,0,0),None


  (* keep array of opponents' resources *)


  let handle_road_building cm b roadpath rl  : move = 
    match roadpath with 
      | hd::tl -> begin
        match tl with
        | rd2::l -> begin
          if List.length (roads_of cm rl) +1 < cMAX_ROADS_PER_PLAYER then Action(PlayCard(PlayRoadBuilding (hd, Some(rd2))))
          else Action(PlayCard(PlayRoadBuilding (hd, None)))
        end 
        | [] -> Action(PlayCard(PlayRoadBuilding (hd, None)))
      end
      | [] -> Action(RollDice)






  (* Invalid moves are overridden in game *)
  let handle_request (g : state) : move =
    let b,pl,t,(cm,r) = g in
    (*(hlist, plist), (il, rl), dk, dis, rob*)
    let (hl, portl), (il, rl), _, _, _ = b in
    let ourplayer = player cm pl in
    let inv = inv_of ourplayer in
    (*================Decisions=================*)
    let target_pt = best_available_pts_on_map b in
    let roadpath = (try (roadlist_to (List.hd target_pt) cm pl rl il)
                    with _ -> []) in
    (match !prev_trade with
      | prev_inv, Some(col) when prev_inv = inv-> (Hashtbl.add scores col ((Hashtbl.find scores col)-1)); (prev_trade:=(empty_cost, None))
      | prev_inv, Some(col)                    -> (Hashtbl.add scores col ((Hashtbl.find scores col)+1)); (prev_trade:=(empty_cost, None))
      | _ -> ());
    (*==========================================*)
    (*DEBUGGING*)
    
    (*==========================================*)
    match r with
      | InitialRequest                                                       -> handle_initial cm b
      | RobberRequest                                                        -> handle_robber cm b pl
      | DiscardRequest                                                       -> DiscardMove(discard_half cm pl)
      | TradeRequest                                                         -> handle_trade t.active t.pendingtrade il pl
      | ActionRequest when is_none t.dicerolled 
                      && valid_play_card RoadBuilding cm pl 
                      && List.length (roads_of cm rl) <cMAX_ROADS_PER_PLAYER
                      && not t.cardplayed                                    -> handle_road_building cm b roadpath rl 
      | ActionRequest when is_none t.dicerolled                              -> Action(RollDice)
      | ActionRequest when not t.cardplayed && have_valid_card cm pl         -> handle_card cm pl b
      | ActionRequest when should_initiate_trade history scores cm pl 
                      && t.tradesmade < cNUM_TRADES_PER_TURN                 -> 
                                let (col, cost1, cost2) = get_some (handle_trade_initiate history scores cm pl) in
                                (prev_trade := inv, Some(col));
                                (history := (col, cost1,cost2)::!history);
                                Action(DomesticTrade(col, cost1, cost2))
      | ActionRequest when not (is_none (best_build_city_now cm b))
                      && can_pay ourplayer cCOST_CITY
                      && num_cities_of cm il < cMAX_CITIES_PER_PLAYER        -> handle_city cm b
      | ActionRequest when not (is_none (best_build_town_now cm b))
                      && can_pay ourplayer cCOST_TOWN
                      && num_towns_of cm il < cMAX_TOWNS_PER_PLAYER          -> handle_town cm b
      | ActionRequest when (is_none (best_build_town_now cm b))
                      && can_pay ourplayer cCOST_ROAD
                      && List.length (roads_of cm rl) <cMAX_ROADS_PER_PLAYER 
                      && List.length roadpath > 0                            -> Action(BuyBuild(BuildRoad(List.hd roadpath)))
      | ActionRequest when valid_build_card cm pl (dk_of b)                  -> Action(BuyBuild(BuildCard))
      | ActionRequest when should_maritime cm pl b                           -> handle_maritime cm pl
      | _ -> next_turn (); Action(EndTurn) 
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
