let can_move_left grid i j =
  j - 1 >= 0 && grid.(i).(j - 1) <> 'X'

let can_move_right grid i j =
  j + 1 < Array.length grid.(0) && grid.(i).(j + 1) <> 'X'

let can_move_up grid i j =
  i - 1 >= 0 && grid.(i - 1).(j) <> 'X'

let can_move_down grid i j =
  i + 1 < Array.length grid && grid.(i + 1).(j) <> 'X'

let possible_paths = ref []

let rec traverse grid sources pending_treasures i j path_count visited =
  let pending_treasures =
    if grid.(i).(j) = 'T' then
      List.filter (fun (x, y) -> (x, y) <> (i, j)) pending_treasures
    else
      pending_treasures
  in

  if grid.(i).(j) = 'S' && List.length pending_treasures = 0 then
    possible_paths := path_count :: !possible_paths;

  if Hashtbl.mem visited (i, j) && Hashtbl.find visited (i, j) = List.length pending_treasures then
    (* give up; futile loop detected *)
    ()
  else
    let new_visited = Hashtbl.copy visited in
    Hashtbl.replace new_visited (i, j) (List.length pending_treasures);

    if can_move_left grid i j then
      traverse grid sources pending_treasures i (j - 1) (path_count + 1) new_visited;

    if can_move_right grid i j then
      traverse grid sources pending_treasures i (j + 1) (path_count + 1) new_visited;

    if can_move_up grid i j then
      traverse grid sources pending_treasures (i - 1) j (path_count + 1) new_visited;

    if can_move_down grid i j then
      traverse grid sources pending_treasures (i + 1) j (path_count + 1) new_visited

let shortest_treasure_hunt grid =
  let sources = ref [] in
  let treasures = ref [] in

  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      if grid.(i).(j) = 'S' then
        sources := (i, j) :: !sources
      else if grid.(i).(j) = 'T' then
        treasures := (i, j) :: !treasures
    done
  done;

  let visited = Hashtbl.create (Array.length grid * (Array.length grid.(0))) in
  match !sources with
  | (src_i, src_j) :: _ ->
      traverse grid !sources !treasures src_i src_j 0 visited;
      if List.length !possible_paths > 0 then
        List.fold_left min max_int !possible_paths
      else
        -1
  | [] -> -1

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
  assert (
    shortest_treasure_hunt [|
    [| 'S'; 'E'; 'X' |];
    [| 'X'; 'T'; 'E' |];
    [| 'E'; 'E'; 'T' |]
  |] = 8 
  );
