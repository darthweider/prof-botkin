open Definition
open Constant
open Util
open Print
open Player

(* subtraction of cost2 from cost1. Returns error if any resources are negative *)
let diff_cost cost1 cost2 : cost =
	map_cost2 (fun a b -> a-b) cost1 cost2

let add_cost cost1 cost2 : cost =
	map_cost2 (fun a b -> a+b) cost1 cost2	

(* c can pay fee *)
let can_pay (p : player ) (fee : cost) =
	try (fun x -> true) (diff_cost (inv_of p) fee)
	with _ -> false

let random_resource () : resource =
	match Random.int 4 with
	| 0 -> Brick
	| 1 -> Wool
	| 2 -> Ore
	| 3 -> Grain
	| 4 -> Lumber
	| _ -> failwith "invalid random number"

(* if the number of resources in dis is the floor of half the resources that color c owns *)
let valid_discard c dis pl : bool =
	sum_cost dis = (sum_cost (inv_of (player c pl))) / 2

(* returns a random cost of n resources to discard from inventory inv *)
let random_discard inv : move =
	(* d is the cost to discard *)
	let rec help_discard n inv d : cost =
		if n = 0 then d
		else
			let ran_rsc = random_resource () in
			(* if player has this random resource, add it to the discard list *)
			if num_resource_in_inventory inv ran_rsc > 0 then  
				let c = single_resource_cost ran_rsc in
				help_discard (n-1) (diff_cost inv c) (add_cost d c)
			else help_discard n inv d in
	DiscardMove(help_discard ((sum_cost inv) / 2) inv (0,0,0,0,0))
