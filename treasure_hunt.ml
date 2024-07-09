(*
123
"from typing import List

def shortest_treasure_hunt(grid: List[List[str]]) -> int:
    """"""
    Given a grid representing a map, finds the shortest path to collect all treasures and return to the starting point.

    Args:
        grid (List[List[str]]): A 2D list of characters where 'S' represents the start point, 'T' represents a treasure, 
                                'E' represents an empty cell, and 'X' represents an obstacle.

    Returns:
        int: The length of the shortest path to collect all treasures and return to the start point. 
             Returns -1 if no path is found.

    Examples:
        >>> grid = [
        ...     ['S', 'E', 'T'],
        ...     ['X', 'T', 'E'],
        ...     ['E', 'E', 'T']
        ... ]
        >>> shortest_treasure_hunt(grid)
        8
    """"""
" *)



let can_move grid i j =
  i >= 0 && i < Array.length grid &&
  j >= 0 && j < Array.length grid.(0) &&
  grid.(i).(j) <> 'X'

(* Helper function to remove an element from a list *)
let rec remove elem = function
  | [] -> []
  | hd :: tl -> if hd = elem then tl else hd :: remove elem tl

let shortest_treasure_hunt grid =
  let sources = ref [] in
  let treasures = ref [] in

  (* Collect sources and treasures *)
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      match grid.(i).(j) with
      | 'S' -> sources := (i, j) :: !sources
      | 'T' -> treasures := (i, j) :: !treasures
      | _ -> ()
    done
  done;

  let directions = [(-1, 0); (1, 0); (0, -1); (0, 1)] in
  let to_explore = Queue.create () in

  (* Initialize the queue with all sources *)
  List.iter (fun source ->
    Queue.add (0, !treasures, source) to_explore) !sources;

  let visited = Hashtbl.create 100 in

  let rec bfs () =
    (* If Queue is empty => No more paths *)
    if Queue.is_empty to_explore then -1
    else
      let path_count, pending_treasures, (x, y) = Queue.pop to_explore in
      (* If already visited try next *)
      if Hashtbl.mem visited (x, y, pending_treasures) then bfs ()
      else begin
        (* Else explore neighbourhood *)
        Hashtbl.add visited (x, y, pending_treasures) true;

        let new_pending_treasures =
          if grid.(x).(y) = 'T' then
            remove (x, y) pending_treasures
          else
            pending_treasures
        in

        if grid.(x).(y) = 'S' && new_pending_treasures = [] then
          path_count
        else begin
          List.iter (fun (dx, dy) ->
            let nx, ny = x + dx, y + dy in
            if can_move grid nx ny then
              Queue.add (path_count + 1, new_pending_treasures, (nx, ny)) to_explore
          ) directions;
          bfs ()
        end
      end
  in
  bfs ()


let () =
  assert ( shortest_treasure_hunt [|
    [| 'X'; 'S'; 'X' |];
    [| 'X'; 'E'; 'X' |];
    [| 'X'; 'E'; 'X' |];
    [| 'X'; 'E'; 'X' |];
    [| 'X'; 'E'; 'X' |];
    [| 'X'; 'X'; 'X' |];
    [| 'E'; 'E'; 'T' |]
  |] = -1 );
  (* Has single source *)
  assert (
    shortest_treasure_hunt [|
    [| 'S'; 'E'; 'T' |];
    [| 'X'; 'T'; 'E' |];
    [| 'E'; 'E'; 'T' |]
  |] = 8 
  );
  (* Has loop + a treasure from seconds source *)
  assert (
    shortest_treasure_hunt [|
      [|'S'; 'E'; 'X'; 'X'; 'X'; 'X'|];
      [|'X'; 'E'; 'E'; 'E'; 'E'; 'X'|];
      [|'X'; 'E'; 'X'; 'X'; 'E'; 'X'|];
      [|'X'; 'E'; 'X'; 'X'; 'E'; 'X'|];
      [|'X'; 'E'; 'E'; 'E'; 'E'; 'X'|];
      [|'X'; 'X'; 'X'; 'X'; 'X'; 'X'|];
      [|'E'; 'X'; 'X'; 'T'; 'S'; 'E'|]
  |] = 2 
  ); 
  (* Has multiple source *) 
  assert (
    shortest_treasure_hunt [|
      [|'X'; 'S'; 'X'|];
      [|'X'; 'E'; 'X'|];
      [|'X'; 'E'; 'S'|];
      [|'S'; 'E'; 'X'|];
      [|'X'; 'E'; 'X'|];
      [|'X'; 'X'; 'X'|];
      [|'E'; 'S'; 'T'|]
  |] = 2 
  );
  (* Treasure is inaccesible *) 
  assert (
    shortest_treasure_hunt [|
      [|'X'; 'S'; 'X'|];
      [|'X'; 'E'; 'X'|];
      [|'X'; 'E'; 'X'|];
      [|'X'; 'E'; 'X'|];
      [|'X'; 'E'; 'X'|];
      [|'X'; 'X'; 'X'|];
      [|'E'; 'E'; 'T'|]
  |] = -1 
  );
