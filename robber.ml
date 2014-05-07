open Definition
open Constant
open Util
open Print
open Printf
open Structures

(* placing the robber on piece pc is a valid move on board b *)
(* robber is not currently at the piece indicated.
   If a color is indicated, it must have a settlement on an adjacent pt.
   If a color is not indicated, there must be no colors adjacent to pc. *)
let valid_rob (pc,copt) b =
	let m, (il, rl), dk, dis, rob = b in
	if pc = rob then false
	else 
		match copt with
		| None when colors_near pc il = []            -> true
		| Some(c) when List.mem c (colors_near pc il) -> true
		| _                                           -> false

let rec random_rob c b : move =
	let _,(il,_),_,_,rob = b in
	let ran_pc = Random.int cMAX_PIECE_NUM in
	let ran_col = pick_random (colors_near ran_pc il) in
	match ran_col with
	| Some(col) when col != c && ran_pc != rob -> RobberMove(ran_pc, ran_col)
	| _ -> random_rob c b