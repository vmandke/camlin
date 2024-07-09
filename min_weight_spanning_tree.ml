(*
125
"from typing import List

def minimumPathSum(N: int, M: int, edges: List[List[int]]) -> int:
    """"""
    Given two integers, N and M, representing the number of nodes and edges in a graph, respectively. A list of size M
    contains sublists, each with three values: the nodes connected by an edge and the edge weight. The goal is to remove
    some edges (if possible) to satisfy the following conditions:

    1. It is still possible to reach from any node to any other node.
    2. The sum of weights of remaining edges is as minimum as possible.
    3. All remaining edge weights must be unique.

    Returns the sum of edge weights of the remaining edges.

    Args:
        N (int): The number of nodes in the graph.
        M (int): The number of edges in the graph.
        edges (List[List[int]]): A list of edges where each edge is represented by a sublist of three integers [u, v, w]
                                 where u and v are nodes connected by an edge with weight w.

    Returns:
        int: The sum of weights of the remaining edges after removal.

    Examples:
        >>> minimumPathSum(3, 3, [[1, 3, 1], [2, 3, 2], [1, 3, 10]])
        3
        
        >>> minimumPathSum(4, 6, [[1, 2, 1], [1, 3, 1], [1, 4, 1], [2, 3, 1], [2, 4, 1], [3, 4, 1]])
        3
    """"""
" *)

(* Function to check if all edge weights in a list of edges are unique *)
  let all_edge_weights_are_unique edges =
    let weights = List.map (fun e -> e.(2)) edges in
    let unique_weights = List.sort_uniq compare weights in
    List.length weights = List.length unique_weights
  
  (* Function to visit an edge in the MST calculation *)
  let rec visit_edge nodes_count edge_count mst_edges dropped_edges visited edge all_seen_path_sums =
    let visited_new = visited |> List.append [edge.(0); edge.(1)] |> List.sort_uniq compare in
    let mst_edges_new = List.append mst_edges [edge] in
    let dropped_edges_new = List.filter (fun e -> e != edge) dropped_edges in
    let seen_path_sums =
      if all_edge_weights_are_unique mst_edges_new then
        minimum_spanning_tree nodes_count edge_count mst_edges_new dropped_edges_new visited_new all_seen_path_sums
      else
        all_seen_path_sums
    in
    all_seen_path_sums @ seen_path_sums
  
  (* Function to find the minimum spanning tree (MST) *)
  and minimum_spanning_tree nodes_count edge_count mst_edges dropped_edges visited all_seen_path_sums =
    if List.length visited = nodes_count then
      let sum_of_edges = List.fold_left (fun acc e -> acc + e.(2)) 0 mst_edges in
      all_seen_path_sums @ [sum_of_edges]
    else
      let rec traverse_dropped_edges = function
        | [] -> all_seen_path_sums
        | edge :: rest ->
            if List.mem edge.(0) visited && List.mem edge.(1) visited then
              traverse_dropped_edges rest
            else if List.mem edge.(0) visited || List.mem edge.(1) visited then begin
              let seen_path_sums = visit_edge nodes_count edge_count mst_edges dropped_edges visited edge all_seen_path_sums in
              let all_seen_path_sums = all_seen_path_sums @ seen_path_sums in
              let other_edges = List.filter (fun de -> de.(0) = edge.(0) && de.(1) = edge.(0) && de.(2) != edge.(2)) dropped_edges in
              all_seen_path_sums @ List.fold_left (fun acc e -> visit_edge nodes_count edge_count mst_edges dropped_edges visited e all_seen_path_sums @ acc) [] other_edges
            end
            else
              traverse_dropped_edges rest
      in
      traverse_dropped_edges dropped_edges
  
  (* Function to compute the minimum path sum *)
  let minimum_path_sum nodes_count edge_count edges =
    let edges_sorted = List.sort (fun e1 e2 -> compare e1.(2) e2.(2)) edges in
    let intial_edge = List.hd edges_sorted in
    let initial_visited = [intial_edge.(0); intial_edge.(1)] in
    let mst_edges = [intial_edge] in
    let dropped_edges = List.tl edges_sorted in
    let seen_sums = minimum_spanning_tree nodes_count edge_count mst_edges dropped_edges initial_visited [] in
    match seen_sums with
    | [] -> -1
    | _ -> List.fold_left min max_int seen_sums
  

let () =
  assert (minimum_path_sum 3 3 [ [|1; 3; 1|]; [|2; 3; 2|]; [|1; 3; 10|] ] = 3);
  assert (minimum_path_sum 4 6 [ [|1; 2; 1|]; [|1; 3; 2|]; [|1; 4; 3|]; [|2; 3; 4|]; [|2; 4; 5|]; [|3; 4; 6|]] = 6);
  (* No mst can be formed as all weights are same *)
  assert (minimum_path_sum 4 6 [ [|1; 2; 1|]; [|1; 3; 1|]; [|1; 4; 1|]; [|2; 3; 1|]; [|2; 4; 1|]; [|3; 4; 1|]] = -1); 
