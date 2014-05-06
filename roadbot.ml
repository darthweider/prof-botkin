open Definition
open Registry
open Constant
open Print
open Util
open Player
open Structures
open Roadbfs
(** Give your bot a 2-20 character name. *)
let name = "roadbot"


module Bot = functor (S : Soul) -> struct

(*HELPER FUNCTIONS================================================================*)
(*COPIED FUNCTIONS====================================================================*)

(*====================================================================================*)



(*================================================================================*)

  let build_road = ref(true)
  let build_town = ref(true)
  let turn_count = ref(0)
  let reset_turn = ref(0)
  (* If you use side effects, start/reset your bot for a new game *)
  let initialize () = build_road:=true; 
                      build_town:=true;
                      turn_count:=0;
                      reset_turn:=0

  let new_turn () = build_road:=true; 
                    build_town:=true

  (* Invalid moves are overridden in game *)
  let handle_request ((b,pl,t,n) : state) : move =
    let (_, _), (il, rl), _, _, _ = b in
    let (c, r) = n in
    reset_turn:= (if !reset_turn = !turn_count then !reset_turn
                  else ((new_turn()); !reset_turn+1));
    let rd_start = match roads_of c rl with 
      | (_,(s, e))::tl -> max s e 
      | _ -> Random.int 53 in

    let test_path = shortest_path_to 53 c pl [(rd_start, None)] [] [] rl il in
    print_string ((string_of_color c) ^ "\n");
    print_string ("Starting Point : " ^ (string_of_int rd_start));
    print_string (string_of_list (fun (_,(pt1, pt2)) -> "Road : " ^ (string_of_int pt1) ^ " to " ^ (string_of_int pt2) ^ "\n") test_path);

    let town_pt = match roads_of c rl with 
      | (_,(s, e))::tl -> max s e 
      | _ -> Random.int 53 in
      print_string ("Reset: " ^ (string_of_int !reset_turn) ^ " Turn: " ^ (string_of_int !turn_count) ^ "  ");
      (if !build_road then print_string "Build_Road : True "
        else print_string "Build_Road : False ");
      (if !build_town then print_string "Build_Town : True "
        else print_string "Build_Town : False ");
    match r with
      | InitialRequest -> InitialMove(0, 0)
      | RobberRequest -> RobberMove(0, None)
      | DiscardRequest-> DiscardMove(0,0,0,0,0)
      | TradeRequest -> TradeResponse(true)
      | ActionRequest -> 
        if is_none t.dicerolled then Action(RollDice) else begin
          match !build_road , !build_town with
          | true , _ when valid_build_road c pl (c, (rd_start, rd_start+1)) rl il cCOST_ROAD-> build_road:=false; (print_string ("Trying to build a road from" ^(string_of_int rd_start)^ " to " ^ (string_of_int (rd_start+1)))); Action(BuyBuild(BuildRoad(c, (rd_start, rd_start+1))))
          | _, true  when valid_build_town c town_pt pl rl il->  build_town:=false; print_string ("Tring to build town at" ^ (string_of_int town_pt)); Action(BuyBuild(BuildTown(town_pt)))
          | _ -> turn_count:= !turn_count+1; Action(EndTurn)
        end 


end


(* Do not change *)
let _ = register_bot name
  (module Bot(New)) (module Bot(New)) (module Bot(New)) (module Bot(New))
