open Definition
open Registry
open Constant
open Util
open Structures
open Roadbfs
open Bot_settlements
open Bot_robber
open Bot_discard
open Player
open Robber

(** Give your bot a 2-20 character name. *)
let name = "ProfBotkin"


module Bot = functor (S : Soul) -> struct
  (*Our refs*)
  let played_card = ref(false) 
  let num_player_trades = ref(0)
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () =
    played_card:=false;
    num_player_trades:=0


  (*Reset any turn-based refs*)
  let next_turn () = ()

  (* keep array of opponents' resources *)


  let rec handle_initial cm b : move =
    let tentative_ln = 
      match best_available_pts_on_map b with
      | best1::t -> (best1, random_adj_pt best1)
      | [] -> random_line in
    if valid_initial cm tentative_ln b then InitialMove(tentative_ln)
    else handle_initial cm b

  let handle_robber cm b pl : move =
    let il = il_of b in
    match best_rob_pieces2 cm b with
    | pc::t -> RobberMove(pc, (best_steal_from pc cm il pl) )
    | [] ->
      match best_rob_pieces1 cm b with
      | pc::t -> RobberMove(pc, (best_steal_from pc cm il pl) )
      | [] -> random_rob cm b

  let handle_road_building cm b roadpath : move = 
    match roadpath with 
      | hd::tl -> begin
        match tl with
        | rd2::l -> Action(PlayCard(PlayRoadBuilding (hd, Some(rd2))))
        | [] -> Action(PlayCard(PlayRoadBuilding (hd, None)))
      end
      | [] -> Action(RollDice)

  (*Can we build a town given our current set of roads? If so, build NO MORE roads*)
  let build_more_roads cm b : bool =
    match best_build_town_now cm b with
    | Some(_) -> false
    | _ -> true

  let have_valid_card (cm:color) pl = 
    let vd_cards = [Knight; RoadBuilding; YearOfPlenty; Monopoly] in
    let p = player cm pl in
    let hnd = reveal (cards_of p) in
    List.exists (fun cd -> fst(have_card_of cd hnd)) vd_cards

  (* Invalid moves are overridden in game *)
  let handle_request (g : state) : move =
    let b,pl,t,(cm,r) = g in
    (*(hlist, plist), (il, rl), dk, dis, rob*)
    let (_, _), (il, rl), _, _, _ = b in

    (*================Decisions=================*)
    let target_pt = best_available_pts_on_map b in
    let roadpath = (try (roadlist_to (List.hd target_pt) cm pl rl il)
                    with _ -> []) in
    (*==========================================*)


    match r with
      | InitialRequest -> handle_initial cm b
      | RobberRequest -> handle_robber cm b pl
      | DiscardRequest-> discard_half cm pl
      | TradeRequest -> TradeResponse(true)
      | ActionRequest when is_none t.dicerolled 
                      && valid_play_card RoadBuilding cm pl 
                      && List.length (roads_of cm rl) <cMAX_ROADS_PER_PLAYER -> handle_road_building cm b roadpath
      | ActionRequest when is_none t.dicerolled -> Action(RollDice)
      | ActionRequest when not t.cardplayed && have_valid_card cm pl-> Action(EndTurn)
      | _ -> Action(EndTurn) 
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
