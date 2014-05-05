open Definition
open Registry
open Constant
open Util

(** Give your bot a 2-20 character name. *)
let name = "roadbot"


module Bot = functor (S : Soul) -> struct

  let build = ref(true)
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = build:=true

  (* Invalid moves are overridden in game *)
  let handle_request ((_,p,t,n) : state) : move =
    let (c, r) = n in
    let rd_start = Random.int 53 in
    match r with
      | InitialRequest -> InitialMove(0, 0)
      | RobberRequest -> RobberMove(0, None)
      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeResponse(true)
      | ActionRequest -> 
        if is_none t.dicerolled then Action(RollDice) else begin
          if !build then Action(BuyBuild(BuildRoad(c, (rd_start, rd_start+1))))
        else Action(EndTurn)
        end 
end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
