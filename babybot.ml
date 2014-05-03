open Definition
open Registry
open Constant
open Util
open Player

(** Give your bot a 2-20 character name. *)
let name = "babybot"


module Bot = functor (S : Soul) -> struct
  (* If you use side effects, start/reset your bot for a new game *)

  let tradeonce = ref(true)
  let initialize () = tradeonce:=true; ()

  (* Invalid moves are overridden in game *)
  let handle_request ((_,p,t,n) : state) : move =
    let (c, r) = n in
    match r with
      | InitialRequest -> InitialMove(0, 0)
      | RobberRequest -> RobberMove(0, None)
      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeResponse(true)
      | ActionRequest -> if is_none t.dicerolled then begin
        tradeonce:=true;
        print_string "Rolled";
        Action(RollDice)
        end
        else 
          if !tradeonce then begin
            print_string "Trade";
            (tradeonce:=false); Action(DomesticTrade((next_turn c), (1,0,0,0,0), (0,1,0,0,0)))
          end
          else Action(EndTurn) 
          (*
        if is_none t.dicerolled then (!tradeonce := true); Action(RollDice) 
        else Action(RollDice) 
          if !tradeonce then (!tradeonce:=false); Action(DomesticTrade((next_turn c), (1,0,0,0,0), (0,1,0,0,0)))
          else Action(EndTurn)  *)

end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
