open Definition
open Registry
open Constant
open Util

let road_exists (s,e) rl = 
	List.exists (fun (c,(pt1, pt2)) -> (pt1 = s && pt2 = e) || (pt1 = e && pt2 = s)) rl

(*NOTE: Frontier should be initialized as the list containing only our starting point, None
	The format of nodes in this algorithm is (CURRENT_NODE, SOME(PREVIOUS_NODE))*)
let rec shortest_path_to (target : point) (c : color) (path : road list) (frontier : (point * point option) list) (backup_frontier : (point * point option) list) (seen_points : point list) (rl : road list) =
	match frontier with 
		| (pt,prev)::remaining_frontier -> begin
			let newpath = match prev with
				| Some(pre_pt) -> (c, (pre_pt, pt))::path
				| _ -> path in
			let seen_points = (pt::seen_points) in
			if pt = target then newpath 
			else begin
				(*Adjacent nodes are points that are adjacent to pt, but not in seen_points*)
				let adjNodes = List.filter (fun x -> not (List.mem x seen_points)) (adjacent_points pt) in
				(*Adjacent nodes can't already have a road between pt and node*)
				let adjNodes = List.fold_left (fun acc x -> if road_exists (x, pt) rl then acc else x::acc) [] adjNodes in
				(*Add the parent to each Node*)
				let adjNodes = List.map (fun x -> (x,Some(pt))) adjNodes in
				let backup_frontier = backup_frontier@adjNodes in
				(*We've created the new path, added nodes to the new frontier, added pt to seen_points,  and are ready to recurse*)
				shortest_path_to target c newpath remaining_frontier backup_frontier seen_points rl 
			end
			
		end
		| [] -> begin
			match backup_frontier with
				(*Move all the points from *)
				| pt::remaining_frontier -> shortest_path_to target c path backup_frontier [] seen_points rl
				(*If we have no nodes in frontier and no nodes in our backup frontier, the point is unreachable*)
				| _ -> []
		end 
