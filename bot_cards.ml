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


    (*Verifies that we have a playable card in our hand*)
  let have_valid_card (cm:color) pl  : bool= 
    (*List of playable cards*)
    let p = player cm pl in
    let vd_cards = [Knight; RoadBuilding; YearOfPlenty; Monopoly] in
    let rs_list = [Brick; Wool; Ore; Grain; Lumber] in
    let valid_monop = List.exists (fun res -> can_pay p (single_resource_cost res)) rs_list in
    let hnd = reveal (cards_of p) in
    (*If the card is a monopoly, verify we have at least one resource*)
    List.exists (fun cd -> fst(have_card_of cd hnd) && (if cd = Monopoly then valid_monop else true)) vd_cards

  let select_valid_card (cm : color ) (pl : player list) : card = 
    let vd_cards = [Monopoly; YearOfPlenty; Knight] in
    let p = player cm pl in
    let hdn = reveal (cards_of p) in
    List.find (fun c -> List.mem c hdn) vd_cards