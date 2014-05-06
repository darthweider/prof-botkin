open Definition
open Registry
open Constant
open Util

(** Give your bot a 2-20 character name. *)
let name = "roadbot"


module Bot = functor (S : Soul) -> struct

(*HELPER FUNCTIONS================================================================*)
(*Given the list of all roads placed and a color c, returns a list of all the roads of color c*)
let roads_of c roadl = 
  List.filter (fun (col,_) -> col = c) roadl
(*================================================================================*)

  let build = ref(true)
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = build:=true

  (* Invalid moves are overridden in game *)
  let handle_request ((b,p,t,n) : state) : move =
    let (_, _), (_, rl), _, _, _ = b in
    let (c, r) = n in
    let rd_start = match roads_of c rl with 
      | (_,(s, e))::tl -> max s e 
      | _ -> Random.int 53 in
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
