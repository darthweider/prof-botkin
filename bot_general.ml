open Definition
open Registry
open Constant
open Util
open Structures
open Player


(* the odds of rolling n. Returns the numerator of the probability (denominator is 36 *)
let odds_of_roll n =
	match n with
	| 2 | 12 -> 1
	| 3 | 11 -> 2
	| 4 | 10 -> 3
	| 5 | 9 -> 4
	| 6 | 8 -> 5
	| 7 -> 6
	| _ -> 0

(* sort_by f l sorts list l such that elements of l with max value when f is applied 
   are at the front of the list *)
let sort_by f =
	let compare a b =
		let fa = f a in
		let fb = f b in
		if fa < fb then 1
		else if fa > fb then ~-1
		else 0 in
	List.sort (compare)