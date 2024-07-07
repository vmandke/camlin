(* Function to check if sequences are sorted *)
  let are_sequences_sorted sequences =
    let rec check_sorted i =
      if i >= Array.length sequences then true
      else if sequences.(i - 1).(Array.length sequences.(i - 1) - 1) > sequences.(i).(0) then false
      else check_sorted (i + 1)
    in
    check_sorted 1
  
  (* Function to merge sequences once *)
  let merge_sequence_once sequences =
    let seq1 = List.hd sequences in
    let seq2 = List.hd (List.tl sequences) in
    let sequences = List.tl (List.tl sequences) in

    let rec find_merge_point seq1 seq2 merge_point =
      if merge_point < List.length seq1 && List.nth seq1 merge_point < List.hd seq2 then find_merge_point seq1 seq2 (merge_point + 1)
      else merge_point
    in
    let merge_point = find_merge_point seq1 seq2 0 in

    let new_seq1 = (List.filteri (fun i _ -> i < merge_point) seq1) @ seq2 in
    let new_seq2 = List.filteri (fun i _ -> i >= merge_point) seq1 in
    let sequences = if List.length new_seq1 > 0 then new_seq1 :: sequences else sequences in
    let sequences = if List.length new_seq2 > 0 then sequences @ [new_seq2] else sequences in
    sequences
  
let move_min_left sequences =
  let rec aux sequences cost =
    match sequences with
    | [] -> cost, sequences
    | first_seq :: rest ->
      if Array.length first_seq > 0 && first_seq.(0) = 1 then cost, sequences
      else aux (rest @ [first_seq]) 1
  in
  aux sequences 0


  (* Function to calculate minimum operations to sort the array *)
  let min_operations_to_sort arr =
    let sequences = ref [[arr.(0)]] in
    for i = 1 to Array.length arr - 1 do
      let last_seq = List.hd ( List.rev !sequences) in
      if arr.(i) > List.hd ( List.rev last_seq) then
          sequences := List.rev(List.tl (List.rev !sequences)) @ [last_seq @ [arr.(i)]]
        else
          sequences := !sequences @ [[arr.(i)]];
    done;
    let operations = ref 0 in
    let cost, new_sequences = move_min_left (List.map Array.of_list !sequences) in
    sequences := List.map Array.to_list new_sequences;
    operations := !operations + cost;
    while not (are_sequences_sorted (Array.of_list (List.map Array.of_list !sequences))) do
      sequences := merge_sequence_once !sequences;
      incr operations;
    done;
    !operations
  

  
  let () =
    assert (min_operations_to_sort [|1; 2; 4; 5; 3|] = 1);
    assert (min_operations_to_sort [|1; 2; 3; 4|] = 0);
    assert (min_operations_to_sort [|1; 2; 5; 3; 4|] = 1);
    assert (min_operations_to_sort [|3; 2; 1|] = 3); 
    assert (min_operations_to_sort [|2; 1|] = 1); 
