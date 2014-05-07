open Definition
open Registry
open Constant
open Util
open Structures
open Player
open Bot_general


(* returns the resource that color c has the most of.
   PRE: c has resources. *)
let highest_resource c pl : resource =
	let b,w,o,g,l = inv_of (player c pl) in
	let sorted_costs = List.sort (min) (b::w::o::g::[l]) in
	let highest = List.hd sorted_costs in
	     if highest = b then Brick
	else if highest = w then Wool
	else if highest = o then Ore
	else if highest = g then Grain
	else                     Lumber

(* cost of discarding n resources from inventory, based on the resources that are 
   most abundant in inv *)
let discardn n inv c pl: cost =
	let rec helper n inv chosen =
		match n with
		| 0 -> chosen
		| _ -> let (chose : cost) = single_resource_cost (highest_resource c pl) in
			   helper (n-1) (diff_cost inv chose) (add_cost chosen chose) in
	helper n inv empty_cost

(* cost of discarding half of the resources of color c. 
   discards most abundant resources first *)
let discard_half c pl : cost =
	let inv = inv_of (player c pl) in
	let n = (sum_cost (inv)) / 2 in
	discardn n inv c pl
		


