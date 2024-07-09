"package main

// isEqualSumPossible determines if it is possible to distribute elements from the second array to the first array
// such that the sum of elements at odd indexes equals the sum of elements at even indexes in the resultant array.
//
// Args:
// - arr1: Array of positive integers which will be modified.
// - arr2: Array of positive integers whose elements will be picked to add to and modify the first array.
//
// Return: Returns true if the resultant array can have the sum of elements at even indexes equal to the sum of elements
// at odd indexes. Otherwise, returns false.
func isEqualSumPossible(arr1 []int, arr2 []int) bool {"

(* 
ABS(O-E) = X
Divide arr2 into a, b such that a = b + x
a + b = sum(arr2) => 2a = sum(arr2) + X
=> sum(arr2) + X  *)


let is_subset_sum_possible arr total_sum =
  let n = Array.length arr in
  let subset_matrix = Array.make_matrix (n + 1) (total_sum + 1) false in
  for i = 0 to n do
    subset_matrix.(i).(0) <- true
  done;
  for i = 1 to n do
    for j = 1 to total_sum do
      if j < arr.(i - 1) then
        subset_matrix.(i).(j) <- subset_matrix.(i - 1).(j)
      else
        subset_matrix.(i).(j) <- subset_matrix.(i - 1).(j) || subset_matrix.(i - 1).(j - arr.(i - 1))
    done
  done;
  let is_possible = Array.fold_left (fun acc i -> acc || subset_matrix.(i).(total_sum)) false (Array.init (n + 1) (fun i -> i)) in
  let all_paths = ref [] in
  if is_possible then
    let rec find_subsets arr subset_matrix i remaining_sum current_subset =
      if remaining_sum = 0 then
        all_paths := current_subset :: !all_paths
      else begin
        if subset_matrix.(i-1).(remaining_sum) then
          find_subsets arr subset_matrix (i-1) remaining_sum (List.rev_append current_subset []);
        if remaining_sum >= arr.(i-1) && subset_matrix.(i-1).(remaining_sum - arr.(i-1)) then
          find_subsets arr subset_matrix (i-1) (remaining_sum - arr.(i-1)) (arr.(i-1) :: current_subset)
      end
    in
    find_subsets arr subset_matrix n total_sum []
  else ();
  is_possible, !all_paths

let get_other_path arr path =
  List.filter (fun x -> not (List.mem x path)) arr

let is_equal_sum_possible_from_path even odd path arr2 target_sum =
  let even = if List.fold_left (+) 0 even < List.fold_left (+) 0 odd then List.rev_append even path else even in
  let odd = if List.fold_left (+) 0 odd < List.fold_left (+) 0 even then List.rev_append odd path else odd in
  let new_arr2 = get_other_path arr2 path in
  let is_possible, all_paths = is_subset_sum_possible (Array.of_list new_arr2) target_sum in
  if not is_possible then
    false
  else
    List.exists (fun path_new_arr_2 ->
      let other_path_new_arr_2 = get_other_path new_arr2 path_new_arr_2 in
      let diff = abs (List.length path_new_arr_2 - List.length other_path_new_arr_2) in
      let diff1 = List.length even - List.length odd + diff in
      let diff2 = List.length even - List.length odd - diff in
      diff1 = 0 || diff1 = 1 || diff2 = 0 || diff2 = 1
    ) all_paths

let is_equal_sum_possible arr1 arr2 =
  let even_elems = List.filteri (fun i _ -> i mod 2 = 0) (Array.to_list arr1) in
  let odd_elems = List.filteri (fun i _ -> i mod 2 <> 0) (Array.to_list arr1) in
  let pending_sum = List.fold_left (+) 0 even_elems - List.fold_left (+) 0 odd_elems in
  let total_available_sum = List.fold_left (+) 0 (Array.to_list arr2) - pending_sum in
  if total_available_sum < 0 || total_available_sum mod 2 = 1 then
    false
  else
    let is_possible, all_paths = is_subset_sum_possible arr2 pending_sum in
    if is_possible then
      List.exists (fun path -> is_equal_sum_possible_from_path even_elems odd_elems path (Array.to_list arr2) (total_available_sum / 2)) all_paths
    else
      false

      
let () =
  (* Case Not Possible *)
  assert (is_equal_sum_possible [|5; 1; 3; 4|] [|1; 2; 3; 4|] = false);
  assert (is_equal_sum_possible [|1;2;3;4;5|] [|3;1|] = false);
  (* Case Is Possible  *)
  assert (is_equal_sum_possible [|3;2;6|] [|7|] = true);
  (* Not required array 2 *)
  assert (is_equal_sum_possible [|2;3;1|] [|1;2;3|] = true); 

