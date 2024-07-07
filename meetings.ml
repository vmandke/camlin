(* Define a type to represent the state of each point *)
type point_state = {
  mutable position : int;
  destination : int;
  direction : int;
}

(* Function to count meetings *)
let count_meetings source destination =
  (* Calculate the maximum time required *)
  let max_time = List.fold_left2 (fun acc src dest -> max acc (abs (src - dest))) 0 source destination in
  (* Initialize the points state *)
  let points_state = List.map2 (fun src dest ->
    { position = src; destination = dest; direction = if src < dest then 1 else -1 }
  ) source destination in

  (* Initialize the number of meetings *)
  let meetings = ref 0 in

  (* Loop through each time step *)
  for _ = 0 to max_time - 1 do
    (* Get the current positions of all points *)
    let positions = List.map (fun point -> point.position) points_state in
    (* Check for meetings *)
    List.iteri (fun p1 pos1 ->
      List.iteri (fun p2 pos2 ->
        if p1 < p2 && pos1 = pos2 then
          incr meetings
      ) positions
    ) positions;

    (* Update positions of points *)
    List.iter (fun point ->
      if point.position <> point.destination then
        point.position <- point.position + point.direction
    ) points_state;
  done;

  (* Return the total number of meetings *)
  !meetings

(* Main function to demonstrate the usage *)
let () =
  assert (count_meetings [2; 1] [3; 4] = 1);
  assert (count_meetings [-10; -5; -12; -13] [10; 5; 12; 13] = 6);
  assert (count_meetings [1; 3; 5; 7] [2; 4; 6; 8] = 0)
