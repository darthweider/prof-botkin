open Definition
open Registry
open Constant
open Util
open Player
open Structures

let rec reconstruct_path_of (c : color) (pt, prev) (seen_points : (point * point option) list) (path : road list) = 
  match prev with
  | Some(parent) -> begin
    let newpath = (c, (pt, parent))::path in
    try (let next = List.find (fun (pt1, _) -> parent = pt1) seen_points in
       reconstruct_path_of c next seen_points newpath)
    with _ -> []
  end
  | None -> path

let road_exists (s,e) c rl  il= 
(*We need to add the roads in our path so far in order to verify if we can place a road*)
  let roadhere = List.exists (fun (c,(pt1, pt2)) -> (pt1 = s && pt2 = e) || (pt1 = e && pt2 = s)) rl in
  (*let enemyblocking = match (List.nth il s), (List.nth il e) with
        | (Some(col, _), _) when c <> col-> List.length (List.filter (fun (_, (pt1, pt2)) -> pt1 = e || pt2 = e) rl) <> 0
        | (_, Some(col, _)) when c <> col-> List.length (List.filter (fun (_, (pt1, pt2)) -> pt1 = s || pt2 = s) rl) <> 0
        | _ -> true in*)
  roadhere

(*NOTE: Frontier should be initialized as the list containing only our starting point, None
  The format of nodes in this algorithm is (CURRENT_NODE, SOME(PREVIOUS_NODE))*)
let rec shortest_path_to (target : point) (c : color) (pl : player list) (frontier : (point * point option) list) (backup_frontier : (point * point option) list) (seen_points : (point * point option) list) (rl : road list) (il : intersection list)=
  match frontier with 
    | (pt,prev)::remaining_frontier -> begin
      let seen_points = ((pt, prev)::seen_points) in
      if pt = target then reconstruct_path_of c (pt, prev) seen_points [] 
      else begin
        (*Adjacent nodes are points that are adjacent to pt, but not in seen_points*)
        let adjNodes = List.filter (fun x -> not (List.exists (fun (pt1, pt2) -> pt1 = x) seen_points)) (adjacent_points pt) in
        (*Adjacent nodes can't already have a road between pt and node*)
        let adjNodes = List.fold_left (fun acc x -> if valid_build_road c pl (c,(x, pt)) rl il (0,0,0,0,0) then acc else x::acc) [] adjNodes in
        (*Add the parent to each Node*)
        let adjNodes = List.map (fun x -> (x,Some(pt))) adjNodes in
        let backup_frontier = backup_frontier@adjNodes in
        (*We've created the new path, added nodes to the new frontier, added pt to seen_points,  and are ready to recurse*)
        shortest_path_to target c pl remaining_frontier backup_frontier seen_points rl il
      end
      
    end
    | [] -> begin
      match backup_frontier with
        (*Move all the points from *)
        | pt::remaining_frontier -> shortest_path_to target c pl backup_frontier [] seen_points rl il
        (*If we have no nodes in frontier and no nodes in our backup frontier, the point is unreachable*)
        | _ -> []
    end 