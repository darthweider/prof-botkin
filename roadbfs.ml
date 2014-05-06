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

let road_exists (s,e) c rl  il seen_points= 
(*We need to add the roads in our path so far in order to verify if we can place a road*)
	let roadhere = List.exists (fun (c,(pt1, pt2)) -> (pt1 = s && pt2 = e) || (pt1 = e && pt2 = s)) rl in
	let rl = reconstruct_path_of c (s, Some(e)) seen_points rl in
	let croads = roads_of c rl in
	let interfering = match (List.nth il s), (List.nth il e) with
				| (Some(col, _), _) when c <> col-> (List.exists (fun (_, (pt1, pt2)) -> pt1 = e || pt2 = e) croads)
				| (_, Some(col, _)) when c <> col-> (List.exists (fun (_, (pt1, pt2)) -> pt1 = s || pt2 = s) croads)
				| _ -> false in
  roadhere || interfering

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
        let adjNodes = List.fold_left (fun acc x -> if (road_exists (x, pt) c rl il seen_points) then acc else x::acc) [] adjNodes in
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
    (*Given a target point, finds the next best road to get there. Returns None if impossible*)
let road_to (target : point) (c: color) (pl : player list) (rl : road list) (il : intersection list) : road option=
    let myroads = roads_of c rl in
    let paths= if List.length myroads = 0 then [] 
        else List.fold_left (fun acc (_, (pt1, pt2)) -> 
        let path1 = shortest_path_to target c pl [(pt1, None)] [] [] rl il in
        let path2 = shortest_path_to target c pl [(pt2, None)] [] [] rl il in
        path1::(path2::acc)) [] myroads in
        
    let pathlengths = List.map (List.length) paths in
    let ((_,chosenPathInd),_) = if List.length paths = 0 then ((0, -1), -1)
                                else List.fold_left (fun ((minpath, pathindex), index) x -> if x <=minpath && x <> 0 then ((x, index), index+1) 
                                        else ((minpath, pathindex), index+1)) ((cNUM_POINTS, -1),0) pathlengths in
    let chosenpath = if chosenPathInd = -1 then [] else List.nth paths chosenPathInd in

   	match chosenpath with
      | (c, (s, e))::tl -> Some(c, (e,s))
      | [] -> print_string "No Path"; None