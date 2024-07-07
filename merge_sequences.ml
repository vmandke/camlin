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
      if List.nth seq1 merge_point < List.hd seq2 then find_merge_point seq1 seq2 (merge_point + 1)
      else merge_point
    in
    let merge_point = find_merge_point seq1 seq2 0 in


    Printf.printf "Seq1 ::: [%s]\n" (String.concat "; " (List.map string_of_int (List.filteri (fun i _ -> i < merge_point) seq1)));
    Printf.printf "Seq2 ::: [%s]\n" (String.concat "; " (List.map string_of_int seq2));

    let new_seq1 = (List.filteri (fun i _ -> i < merge_point) seq1) @ seq2 in
    let new_seq2 = List.filteri (fun i _ -> i >= merge_point) seq1 in
    let sequences = if List.length new_seq1 > 0 then new_seq1 :: sequences else sequences in
    let sequences = if List.length new_seq2 > 0 then sequences @ [new_seq2] else sequences in

    Printf.printf "Merged sequences: [%s]\n"
      (String.concat "; " (List.map (fun seq -> "[" ^ String.concat "; " (List.map string_of_int seq) ^ "]") sequences));

    sequences
  
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
    while not (are_sequences_sorted (Array.of_list (List.map Array.of_list !sequences))) do
      sequences := merge_sequence_once !sequences;
      incr operations;
    done;
    !operations
  

  
  let () =
  (* assert (min_operations_to_sort [|1; 2; 4; 5; 3|] = 1);
  assert (min_operations_to_sort [|1; 2; 3; 4|] = 0);
  assert (min_operations_to_sort [|1; 2; 5; 3; 4|] = 1);
  assert (min_operations_to_sort [|3; 2; 1|] = 3);  *)
  
  let arr = [|3; 2; 1|] in
  let operations = min_operations_to_sort arr in
  Printf.printf "Minimum operations to sort the array: %d\n" operations