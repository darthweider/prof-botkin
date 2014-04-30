open Definition
open Constant
open Util
open Print

(* Return information given a player p *)
let color_of (c,h,t) : color = c
let inv_of (c, (i,cds), t) : inventory = i
let cards_of (c, (i,cds), t) : cards = cds
let trophs_of (c, h, t) : trophies = t

(* player c pl return the player corresponding to color c in pl *)
let player (c : color) (pl : player list) : player =
	List.find (fun p -> color_of p = c) pl


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
