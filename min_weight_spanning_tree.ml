open Printf

module IntSet = Set.Make(struct
  type t = int
  let compare = compare
end)

let min_path_sum = ref max_int

(* Function to visit an edge *)
let rec visit_edge nodes_count edge_count mst_edges dropped_edges visited edge =
  let visited_new = IntSet.add edge.(0) (IntSet.add edge.(1) visited) in
  let mst_edges_new = edge :: mst_edges in
  let dropped_edges_new = List.filter ((<>) edge) dropped_edges in
  minimum_spanning_tree nodes_count edge_count mst_edges_new dropped_edges_new visited_new

(* Function to find the minimum spanning tree *)
and minimum_spanning_tree nodes_count edge_count mst_edges dropped_edges visited =
  if IntSet.cardinal visited = nodes_count then (
    let sum = List.fold_left (fun acc e -> acc + e.(2)) 0 mst_edges in
    if sum < !min_path_sum then min_path_sum := sum
  ) else
    List.iter (fun edge ->
      if IntSet.mem edge.(0) visited && IntSet.mem edge.(1) visited then ()
      else if IntSet.mem edge.(0) visited || IntSet.mem edge.(1) visited then (
        visit_edge nodes_count edge_count mst_edges dropped_edges visited edge;
        (* If alternate edge available for the same node, try that as well *)
        List.iter (fun other_edge ->
          if other_edge <> edge && other_edge.(0) = edge.(0) && other_edge.(1) = edge.(1) then
            visit_edge nodes_count edge_count mst_edges dropped_edges visited other_edge
        ) dropped_edges
      )
    ) dropped_edges

(* Function to find the minimum path sum *)
let minimum_path_sum nodes_count edge_count edges =
  min_path_sum := max_int;
  let edges_sorted = List.sort (fun a b -> compare a.(2) b.(2)) edges in
  let initial_edge = List.hd edges_sorted in
  let initial_visited = IntSet.of_list [initial_edge.(0); initial_edge.(1)] in
  minimum_spanning_tree nodes_count edge_count [initial_edge] (List.tl edges_sorted) initial_visited;
  !min_path_sum

(* Helper function to convert an array of arrays to a list of lists *)
let array_to_list_of_lists arr =
  Array.to_list (Array.map Array.to_list arr)

(* Main function *)
let () =
  assert (minimum_path_sum 3 3 [ [|1; 3; 1|]; [|2; 3; 2|]; [|1; 3; 10|] ] = 3);
  assert (minimum_path_sum 4 6 [ [|1; 2; 1|]; [|1; 3; 1|]; [|1; 4; 1|]; [|2; 3; 1|]; [|2; 4; 1|]; [|3; 4; 1|]] = 3);