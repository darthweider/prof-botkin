open Definition
open Constant
open Util
open Print
open Printf

(*======================INFORMATION RETRIEVAL=================================*)

(* Return information given a player p *)
let color_of (c,h,t) : color = c
let inv_of (c, (i,cds), t) : inventory = i
let cards_of (c, (i,cds), t) : cards = cds
let trophs_of (c, h, t) : trophies = t

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
	try (fun x -> true) (diff_cost (inv_of p) fee)
	with _ -> false


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
	match Random.int 4 with
	| 0 -> Brick
	| 1 -> Wool
	| 2 -> Ore
	| 3 -> Grain
	| 4 -> Lumber
	| _ -> failwith "invalid random number"

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
			else helper inv n chosen in
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
	sum_cost dis > 7 && sum_cost dis = (sum_cost (inv_of (player c pl))) / 2


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
			print ("distributing " ^ (string_of_resource resource) ^ (sprintf " to pt %i" pt)) ;
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
	print (sprintf "distribute_initial for %i" pt) ; 
	List.fold_left ( fun placc resource -> 
		distribute resource pt il placc ) 
		pl (resources_from (adjacent_pieces pt) hexl)




(* NOT IN USE. PROBABLY BROKEN.
   collects the resources generated by the structure at intersection i on point pt
   when roll is rolled. Add these resources to the appropriate place in player list pl  *)
(* let collect roll i pt b pl : player list =
	let (hexl,_),_,_,_,rob = b in

	match i with
	| None -> pl
	| Some((c,s)) -> 
		(* list of adjacent pieces that might generate resources 
		   does not include an adjacent piece that a robber is on *)
		let adj_pieces = List.filter (fun x -> x != rob) (adjacent_pieces pt) in
		(* list of resources that can be collected for the settlement at this intersection
		   resources may be duplicated, indicating duplicate collection *)
		let resources_to_collect = resources_from adj_pieces roll hexl in
		(* conversion of the resources to collect into a cost type
		   taking into account the difference in collection amt for different settlement types *)
		let cost_to_collect = List.fold_left (fun cost_acc resrc -> 
			add_cost cost_acc (n_resource_cost resrc (settlement_num_resources s)) )
			empty_cost resources_to_collect in
		(* return an updated player list with the cost added to the inventory of 
		   the color at this intersection *)
		add_inv cost_to_collect c pl
*)