open Definition
open Registry
open Constant
open Util
open Structures
open Roadbfs
open Bot_settlements
open Bot_resources
open Player
open Robber
open Print


    (*Verifies that we have a playable card in our hand*)
  let have_valid_card (cm:color) pl  : bool= 
    (*List of playable cards*)
    let p = player cm pl in
    let vd_cards = [Knight; YearOfPlenty; Monopoly] in
    let rs_list = [Brick; Wool; Ore; Grain; Lumber] in
    let valid_monop = List.exists (fun res -> can_pay p (single_resource_cost res)) rs_list in
    let hnd = reveal (cards_of p) in
    (*If the card is a monopoly, verify we have at least one resource*)
    List.exists (fun cd -> fst(have_card_of cd hnd) && (if cd = Monopoly then valid_monop else true)) vd_cards

(*Returns a valid card and FAILS otherwise--assumes we have a valid card in hand*)
  let select_valid_card (cm : color ) (pl : player list) : card = 
    let vd_cards = [Monopoly; YearOfPlenty; Knight] in
    let p = player cm pl in
    let hdn = reveal (cards_of p) in
    print_string (string_of_list (fun x -> string_of_card x) hdn);
    List.find (fun c -> List.mem c hdn) vd_cards

    (*Returns a resource for monopoly FAILS if it can't find a resourse*)
  let mono_rsc (cm : color) (pl : player list) : resource =
    let p = player cm pl in
    let rs_list = [Brick; Wool; Ore; Grain; Lumber] in
    let enemy_rsc = List.fold_left (fun acc (c, (i,_), _) -> if c = cm then acc else (add_cost acc i)) empty_cost pl in
    let (b, w, o, g, l) = enemy_rsc in
    let target_lst = [b; w; o; g; l] in
    let sorted_costs = List.rev (List.sort (compare) target_lst) in
    let rsc_we_have : bool list = (List.map (fun rs -> can_pay p (single_resource_cost rs)) rs_list) in
    let rec helper costlist =
    match costlist with
    | highest::tl -> begin
        print_string "\n";
        print_string (string_of_list (fun x -> if x then "True" else "False") rsc_we_have);
         if highest = b && List.nth rsc_we_have 0 then Brick
    else if highest = w && List.nth rsc_we_have 1 then Wool
    else if highest = o && List.nth rsc_we_have 2 then Ore
    else if highest = g && List.nth rsc_we_have 3 then Grain
    else if highest = l && List.nth rsc_we_have 4 then Lumber
    else helper tl
    end
    | _ -> failwith "Monopoly playing failed--Empty list at end of rec helper" in

    helper sorted_costs



  let play_card_of (cd : card) (cm : color) (pl : player list) (b : board) : move = 
    match cd with 
    | Monopoly     -> print_string ("Playing Monopoly on "^ (string_of_resource (mono_rsc cm pl))^"\n"); Action(PlayCard(PlayMonopoly(mono_rsc cm pl)))
    | YearOfPlenty -> Action(PlayCard(PlayYearOfPlenty(Brick, Some(Lumber))))
    | Knight       -> begin
        let rob = match handle_robber cm b pl with
            | RobberMove(rb) -> rb
            | _ -> failwith "Bad robber move found in play_card_of" in
        Action(PlayCard(PlayKnight(rob)))
    end
    | _            -> failwith "Should not occur" (*End the turn here*) 

  let handle_card cm pl b : move =
    play_card_of (select_valid_card cm pl) cm pl b 