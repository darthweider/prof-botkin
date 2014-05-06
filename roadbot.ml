open Definition
open Registry
open Constant
open Util

(** Give your bot a 2-20 character name. *)
let name = "roadbot"


module Bot = functor (S : Soul) -> struct

(*HELPER FUNCTIONS================================================================*)
(*COPIED FUNCTIONS====================================================================*)
(*======================INFORMATION RETRIEVAL=================================*)

(* Return information given a player p *)
let color_of (c,h,t) : color = c
let inv_of (c, (i,cds), t) : inventory = i
let cards_of (c, (i,cds), t) : cards = cds
let trophs_of (c, h, t) : trophies = t
let num_knights p : int = let (k,_,_) = trophs_of p in k
let has_longest_road p : bool = let (_,longest,_) = trophs_of p in longest
let has_largest_army p : bool = let (_,_,largest) = trophs_of p in largest
let num_res_of (c, (i, cds), t) res = num_resource_in_inventory i res

(* player c pl return the player corresponding to color c in pl *)
let player (c : color) (pl : player list) : player =
  List.find (fun p -> color_of p = c) pl


(*==============================COST/RESOURCE CALCULATIONS====================================*)

(* subtraction of cost2 from cost1. The return value can be negative *)
let diff_cost cost1 cost2 : cost =
map_cost2 (fun a b -> 
    a-b) cost1 cost2

  (* Note from Matthew: I've commented out your code here. I don't think we want it to return an error.
  map_cost2 (fun a b -> 
    let diff = a-b in
    if diff > 0 then diff
    else failwith "cannot have a negative cost") cost1 cost2 *)

let add_cost cost1 cost2 : cost =
  map_cost2 (fun a b -> a+b) cost1 cost2  

(* c can pay fee *)
let can_pay (p : player ) (fee : cost) =
  let inv' = diff_cost (inv_of p) fee in
  let check_for_negatives ((b,w,o,l,g) : cost) : bool =
    b>=0 && w>=0 && o>=0 && l>=0 && g>=0 in
  check_for_negatives inv'


(* n_of r n returns a cost for n of resource r *)
let n_resource_cost resource n : cost =
  match resource with
    | Brick ->  (n,0,0,0,0)
    | Wool ->   (0,n,0,0,0)
    | Ore ->    (0,0,n,0,0)
    | Grain ->  (0,0,0,n,0)
    | Lumber -> (0,0,0,0,n)

let empty_cost = (0,0,0,0,0)

let random_resource () : resource =
  match Random.int 5 with
  | 0 -> Brick
  | 1 -> Wool
  | 2 -> Ore
  | 3 -> Grain
  | _ -> Lumber

(* from an inventory inv, choose n random resources and return the cost of all those resources *)
let n_random_resources inv n : cost =
  let rec helper inv n chosen : cost =
    if n = 0 then chosen
    else
      let ran_rsc = random_resource () in
      (* if player has this random resource, add it to the chosen list *)
      if num_resource_in_inventory inv ran_rsc > 0 then  
        let c = single_resource_cost ran_rsc in
        helper (diff_cost inv c) (n-1) (add_cost chosen c)
      else 
      helper inv n chosen in
  helper inv n (0,0,0,0,0)


(*=======================UPDATE PLAYER INFORMATION=====================*)


(* update information of color c in pl with f.
   f takes a player and returns a player *)
let update (c : color) (pl : player list) f : player list =
  List.map ( fun p -> 
    if c = color_of p then f p 
    else p ) pl

(* give add cards cds to player of color c; returns the updated player list *)
let add_cards new_cds c pl : player list =
  update c pl (fun p -> 
    let added = List.fold_left (fun acc_cards new_cd -> append_card acc_cards new_cd) 
      (cards_of p) (reveal new_cds) in
    (c, (inv_of p, added ), trophs_of p))

(* add cost income to the inventory of color c; returns updated player list *)
let add_to_inv income c pl : player list =
  update c pl (fun p ->
    let inv' = add_cost (inv_of p) income in
    (c, (inv', cards_of p), trophs_of p))

let rm_from_inv expense c pl : player list =
  update c pl (fun p ->
    let inv' = diff_cost (inv_of p) expense in
    (c, (inv', cards_of p), trophs_of p))


(* if the number of resources in dis is the floor of half the resources that color c owns AND the number of resources is greater than 7*)
let valid_discard c dis pl : bool =
  (sum_cost (inv_of (player c pl)))  > 7 && sum_cost dis = (sum_cost (inv_of (player c pl))) / 2


(* returns a random discard move that will discard random resources 
   so that the resulting inv would be the floor of half the current inventory *)
let random_discard inv : move =
  if sum_cost inv > 7 then DiscardMove(n_random_resources inv ((sum_cost inv) / 2) )
  else DiscardMove(0,0,0,0,0)



(* distribute resource to the color at point pt, based on intersection list il.
   Update player list pl accordingly. *)
let distribute resource pt il pl : player list =
    match List.nth il pt with
    | Some(c,s) -> 
      add_to_inv (n_resource_cost resource (settlement_num_resources s)) c pl
    | _ -> pl

(* distribute resource to all points in list pts, based on intersection list il.
   Update player list pl accordingly. *)
let distribute_to_pts pts resource il pl : player list =
  List.fold_left ( fun placc pt -> 
    distribute resource pt il placc ) pl pts 

(* Given a list of pieces, return a list of resources that are generated from the
   pieces given *)
let resources_from pcs hexl : resource list =
  List.fold_left ( fun resrc_acc pc -> 
    let (t,n) = List.nth hexl pc in
    match resource_of_terrain t with
    | None -> resrc_acc
    | Some(r) -> r::resrc_acc ) [] pcs

(* distributes initial resources. 
   Given a point (presumably the second chosen initial town), 
   distribute to the color at that point one of each resource from the surrounding hexes.
   Updates the player list accordingly. *)
let distribute_initial pt b pl : player list =
  let (hexl,_),(il,_),_,_,_ = b in
  List.fold_left ( fun placc resource -> 
    distribute resource pt il placc ) 
    pl (resources_from (adjacent_pieces pt) hexl)


let valid_dom_trade (pinit :color) (ptarget:color) (ocost :cost ) (icost : cost) (pl : player list) (t: turn) : bool = 
  (can_pay (player pinit pl) ocost) && (can_pay (player ptarget pl) icost) && t.tradesmade < cNUM_TRADES_PER_TURN

let rec next_to_discard (cm : color) (pl : player list) (t' : turn) (b: board) : state =
  let pnext = next_turn cm in
  if (sum_cost (inv_of (player pnext pl)))  > 7 then (b, pl, t', ((pnext), DiscardRequest))
  else begin
    if pnext = t'.active then (b, pl, t', ((pnext), RobberRequest))
    else next_to_discard pnext pl t' b
  end

(* Given a color c and an intersection list il, get_ports returns a list of ports owned by this player
  The elements of this list are of the form (exchange rate, type of resource) *)
let get_ports (c : color) (il : intersection list) (port_list : port list) : (int * portresource) list=
  let rec helper port_list acc = 
    match port_list with
      | [] -> acc
      | ((loc, loc'), rate, res_type) ::tl ->
      let cons_if_not_none l x =
        match x with 
        | None -> l
        | Some(col, _) -> if c = col then (rate, res_type)::l
                  else l in
      let port_one =  List.nth il loc in
      let port_two =  List.nth il loc' in 
      let acc = cons_if_not_none acc port_one in
      let acc = cons_if_not_none acc port_two in
      helper tl acc in
  helper port_list []

let best_trade_rate (port_list : (int * portresource) list) (res : resource) : int =
  let rec helper l best_rate =
    match l with
    | [] -> best_rate
    | (r, Any)::tl -> helper tl (min r best_rate)
    | (r, PortResource(rs))::tl -> if rs = res then helper tl (min r best_rate)
                     else helper tl best_rate in
  helper port_list 4

let valid_mari_trade (c : color) (pl : player list) (il : intersection list) (plist : port list) (res : resource) : bool = 
  let ports = get_ports c il plist in
  let rate = best_trade_rate ports res in
  let p = player c pl in
  let cost = n_resource_cost res rate in
  can_pay p cost



let valid_build_card (c: color) (pl : player list) (d: deck) =
  (*Verify that player has enough resources
    Verify that the deck is not empty*)
  let p = player c pl in
  let decksize = match d with 
    | Hidden(x) -> x
    | Reveal(l) -> List.length l in
  can_pay p cCOST_CARD  && decksize > 0

(*Returns a tuple of (bool, card list) where bool is (this card is in our hand) and the card list is the hand with one of card
  (if any) removed*)
let have_card_of (cd : card) (h : card list) : bool * (card list) =
  let rec helper h acc flag = 
  match h with
    | c::tl -> if flag then begin
      if c = cd then helper tl acc false
      else helper tl (c::acc) flag
    end
    else helper tl (c::acc) flag
    | [] -> ((not flag), acc) in
  helper h [] true

let valid_play_card card c pl : bool =
  let p = player c pl in
  let h' = reveal (cards_of p) in
  fst (have_card_of card h')  

let valid_knight (c : color) (pl : player list) : bool =
  (*Player has the card in hand*)
  valid_play_card Knight c pl
  
let valid_monopoly (c : color) (pl : player list) (res : resource) : bool =
  let p = player c pl in
  let cost = n_resource_cost res 1 in
  (*Player has at least one of said resource AND has a monopoly card in their hand*)
  can_pay p cost && valid_play_card Monopoly c pl

let valid_year (c : color) (pl : player list) : bool =
  (*Player has the card in hand*)
  valid_play_card YearOfPlenty c pl



let indexed (l : 'a list) : (int * 'a) list =
  let _, indexed = List.fold_left ( fun (ct,lacc) x -> ct+1, lacc @ [(ct, x)] ) (0, []) l in
  indexed



(*==================ROADS======================*)

(*Given the list of all roads placed and a color c, returns a list of all the roads of color c*)
let roads_of c roadl = 
  List.filter (fun (col,_) -> col = c) roadl

(*Given a list of roads and a color, returns the list of roads that could potential (or have already been) placed by a single player*)
let all_possible_roads (roadl : road list) (c : color) : road list =
  let rec helper roadl acc = 
    match roadl with
    | (_,(beg,e))::tl ->
      (*make a list of lines, denoted as (beg, e) tuples*)
      let adjstart = List.map (fun x -> (c,(beg, x))) (adjacent_points beg) in
      let adjend = List.map (fun x -> (c,(e, x))) (adjacent_points e) in
      (*merge the two lists. Duplicates will not mattter. Recurse*)
      helper tl (adjstart@adjend@acc)
    | _ -> acc in 
  helper roadl []



(*==================SETTLEMENTS=====================*)
let num_towns_of c il : int =
  list_count ( fun i -> 
    match i with 
    | Some(color,s) when color = c -> true 
    | _ -> false ) il

let num_cities_of c il : int =
  list_count (fun i -> 
    match i with
    | Some(col, s) when s = City && col = c-> true
    | _ -> false) il

let num_towns_total pl il : int =
  List.fold_left (fun nacc p -> (num_towns_of (color_of p) il) + nacc) 0 pl



(*================ADDING/BUILDING THINGS===============*)



(*In this method we assume that rd is a valid road placement. Places the road*)
let add_road rd rl : road list =
  (*let c,(pt1,pt2) = rd in*)
  rd::rl
  (* print (sprintf "road at %i and %i" pt1 pt2); *)

(* add_road c l rl adds a road on line l for color c to the road list rl.
   Fails if a road already exists on l *)
let initial_add_road c ln rl : road list =
  let pt1,pt2 = ln in
  if not (List.mem pt1 (adjacent_points pt2)) ||
    List.exists (fun (_,(rpt1, rpt2)) -> (rpt1,rpt2) = ln || (rpt2,rpt1) = ln ) rl
    then failwith "Cannot make a road here."
  else (c, ln)::rl
  (* print (sprintf "road at %i and %i" pt1 pt2); *)



(* add a town for color c at point pt on intersection list il.
   Raises an exception if there is a previous settlement at that point or
   if the surrounding area is not clear of settlements *)
let add_town c pt il : intersection list =
  let empty_land = pt :: (adjacent_points pt) in
  let indexed_intersections = indexed il in

  List.fold_left (fun ilacc (loc, i) ->
    match i with
    | Some(s) when List.mem loc empty_land ->
        failwith "Cannot place town here. Area is populated by an existing settlement."
    | None when loc = pt ->  ilacc @ [Some (c,Town)]
    | _                  ->  ilacc @ [i] )
    [] indexed_intersections

(*Adds a city at the given index. Assumes that this is a valid move (does not check if it is not)*)
let add_city c pt il : intersection list = 
  let indexed_intersections = indexed il in
  List.fold_left (fun ilacc (loc, i) -> 
    if loc = pt then ilacc @ [Some(c, City)]
    else ilacc @ [i]) [] indexed_intersections

(* adds an initial road and an initial town *)
let initial c (pt1,pt2) b : board =
  let m, (il, rl), dk, dis, rob = b in
  let structures' = try ( (add_town c (pt1) il), (initial_add_road c (pt1, pt2) rl) ) 
                  with _ -> failwith "Failed to place initial road and settlement." in
  (m, structures', dk, dis, rob)

let valid_point pt =
  0 <= pt && pt <=cMAX_POINT_NUM

let valid_pc pc = 
  0 <= pc && pc <=cMAX_PIECE_NUM


(* If c can built a road on line. The road cost "cost"--that way we can recycle function for placing free roads *)
let valid_build_road c pl desiredr roadl il cost=
  let (targetcol,(s, e)) = desiredr in
  let croads = roads_of c roadl in
  let p = player c pl in
  (*if target color = c AND if player can afford to build a road AND if a road does not exist at the desired location AND number of roads < max number of roads*)
  if c=targetcol 
    && (can_pay p cost) 
    && List.length (List.filter (fun (c, (a,b)) -> (a = s && b = e) || (a = e && b = s)) roadl) = 0 
    && List.length croads < cMAX_ROADS_PER_PLAYER
    && valid_point s && valid_point e then
    begin
      (*check if road is valid*)
      let poss_roads = all_possible_roads croads c in
      (*Check end points to see if we're trying to build over an enemy settlement. If we are, check that the other end of our road
        meets with another one of our roads*)
      let not_interfering = match (List.nth il s), (List.nth il e) with
        | (Some(col, _), _) when c <> col-> List.length (List.filter (fun (_, (pt1, pt2)) -> pt1 = e || pt2 = e) roadl) <> 0
        | (_, Some(col, _)) when c <> col-> List.length (List.filter (fun (_, (pt1, pt2)) -> pt1 = s || pt2 = s) roadl) <> 0
        | _ -> true in
      List.length (List.filter (fun x -> (snd x) = (snd desiredr)) poss_roads) <> 0 && not_interfering
    end
  else false
  (* not an existing road: not in board's structure's road list *)
  (* adjacent to a road or town of this player--This is the same as adjacent to a road of this player *)
  (* have not exceeded max roads per player *)
let valid_build_town c pt pl roadl il=
  let p = player c pl in
  let croads = roads_of c roadl in
  let pathexists = List.exists (fun (col, (s, e)) -> s=pt || e=pt) croads in
  let adj = adjacent_points pt in
  if valid_point pt then begin
    (*Is the target settlement empty, and are there no adjacent settlements?*)
    let empty_in_and_around = List.for_all (fun x -> (List.nth il x) = None) (pt::adj) in
    (*we can afford a town AND we have not exceeded max # of towns AND the target+adjacent squares=empty AND road leads to point*)
    can_pay p cCOST_TOWN && num_towns_of c il < cMAX_TOWNS_PER_PLAYER && empty_in_and_around && pathexists  
  end
  else false


let valid_build_city (c : color) (pt : point) (pl : player list) (il : intersection list) : bool =
  let p = player c pl in
  if valid_point pt then begin
    match List.nth il pt with
    | Some (col, s) when s=Town ->
        (*Town is of correct color AND we can afford a city AND we have not exceeded max num of cities*)
         col = c && can_pay p cCOST_CITY && (num_cities_of c il) < cMAX_CITIES_PER_PLAYER
    | _ -> false
  end
  else false

(*Verify that the road building card can be played*)
let valid_road_building (c: color) (pl: player list) (rl : road list) (il : intersection list) (road1 : road) (road2 : road option) = 
  let firstvalid = valid_build_road c pl road1 rl il (0,0,0,0,0) in
  (*If we're only building one road*)
  if is_none road2 then begin
    firstvalid && valid_play_card RoadBuilding c pl
  end
  (*If we're building both roads*)
  else (
    let road2 = get_some road2 in
    let secondvalid = valid_build_road c pl road2 rl il (0,0,0,0,0) in
    match (firstvalid, secondvalid) with
    | true, false -> begin
      (*Check if placing first road makes second road valid*)
      let rl' = add_road road1 rl in
      (valid_play_card RoadBuilding c pl) &&valid_build_road c pl road2 rl' il (0,0,0,0,0)
    end
    | false, true -> begin
      (*Check if placing second road makes first road valid*)
      let rl' = add_road road2 rl in
      (valid_play_card RoadBuilding c pl) && valid_build_road c pl road1 rl' il (0,0,0,0,0)
    end
    | true, true ->begin
      (*We pick a valid road (either in this case) and see if we can place both*)
      let rl' = add_road road2 rl in
      (valid_play_card RoadBuilding c pl) && valid_build_road c pl road1 rl' il (0,0,0,0,0)
    end
    | _ -> false
  )


let valid_initial c ln b : bool =
  try (fun x -> true) (initial c ln b)
  with _ -> false

let rec random_initialmove c b : move =
  let rand_pt1 = Random.int cNUM_POINTS in
  let rand_pt2 = get_some (pick_random (adjacent_points rand_pt1)) in
  let rand_ln = (rand_pt1, rand_pt2) in
  if valid_initial c rand_ln b then InitialMove(rand_ln)
  else random_initialmove c b
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
