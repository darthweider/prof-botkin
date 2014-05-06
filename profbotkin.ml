open Definition
open Registry
open Constant
open Util
open Structures
open Roadbfs
open Bot_settlements

(** Give your bot a 2-20 character name. *)
let name = "ProfBotkin"


module Bot = functor (S : Soul) -> struct
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = ()

  (* keep array of opponents' resources *)


  let rec handle_initial cm b : move =
    let tentative_ln = 
      match best_available_pts_on_map b with
      | best1::t -> (best1, random_adj_pt best1)
      | [] -> random_line in
    if valid_initial cm tentative_ln b then InitialMove(tentative_ln)
    else handle_initial cm b


  (* Invalid moves are overridden in game *)
  let handle_request (g : state) : move =
    let b,pl,t,(cm,r) = g in
    match r with
      | InitialRequest -> handle_initial cm b
      | RobberRequest -> RobberMove(0, None)
      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeResponse(true)
      | ActionRequest -> 
        if is_none t.dicerolled then Action(RollDice) else Action(EndTurn)
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
