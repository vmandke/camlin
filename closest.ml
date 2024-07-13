(* 
120
"package main

import (
	""fmt""
	""math""
	""sort""
)

// Given an array of integers `arr` and an integer `target`,
// Return the set of integers from the array whose sum is closest or equal to `target` with the least number of elements in ascending order.
// If there are multiple sets with the same closest sum, return the set with the maximum smaller numbers.
//
// Args:
// - arr ([]int): The input array of integers.
// - target (int): The target sum.
//
// Returns:
// - []int: A set of integers whose sum is closest to `target`.
//
// Example:
// closestMinimumElementsSum([]int{1, 3, 4, 7, 10}, 15) => [1, 4, 10]
// closestMinimumElementsSum([]int{1, -3, 1, 1, 9, -3}, 3) => [-3, -3, 9]
// closestMinimumElementsSum([]int{0, 0, 0, 0}, 5) => [0]
func closestMinimumElementsSum(arr []int, target int) []int {" *)



let closest_minimum_elements_sum arr target =

  if List.length arr = 0 then
    []
  else
  let arr = Array.of_list arr in
  Array.sort compare arr;
  let n = Array.length arr in
  let closest_sum = ref max_int in
  let closest_set = ref [] in

  let abs x = if x < 0 then -x else x in

  let rec compare_sets set1 set2 = 
    match set1, set2 with
    | [], [] -> false
    | [], _ -> true
    | _, [] -> false
    | x1::xs1, x2::xs2 -> 
        if x1 < x2 then true
        else if x1 > x2 then false
        else compare_sets xs1 xs2
  in

  let rec find_closest_set start current_sum current_set =
    if abs (current_sum - target) < abs (!closest_sum - target) ||
       (abs (current_sum - target) = abs (!closest_sum - target) && List.length current_set < List.length !closest_set) ||
       (abs (current_sum - target) = abs (!closest_sum - target) && List.length current_set = List.length !closest_set && compare_sets current_set !closest_set) then
      (closest_sum := current_sum;
       closest_set := List.rev current_set);

    for i = start to n - 1 do
      let new_sum = current_sum + arr.(i) in
      find_closest_set (i + 1) new_sum (arr.(i) :: current_set)
    done
  in

  find_closest_set 0 0 [];
  if !closest_set = [] then
    closest_set := [arr.(0)];
  List.sort compare !closest_set

(* Example usage *)
let () =
  assert (closest_minimum_elements_sum [1; 3; 4; 7; 10] 15 = [1; 4; 10]);
  assert (closest_minimum_elements_sum [1; -3; 1; 1; 9; -3] 3 = [-3; -3; 9]);
  assert (closest_minimum_elements_sum [0; 0; 0; 0] 5 = [0]);
  (* Test case: Empty array *)
  assert (closest_minimum_elements_sum [] 5 = []);

  (* Test case: Target is zero *)
  List.iter (fun sum -> Printf.printf "%d\n" sum) (closest_minimum_elements_sum [1; 2; 3; -3; -5; 6] 0);
  assert (closest_minimum_elements_sum [1; 2; 3; -3; -2; -1] 0 = [-1; 1]);

  (* Test case: All elements are positive and target is greater than the sum of all elements *)
  assert (closest_minimum_elements_sum [1; 2; 3; 4] 20 = [1; 2; 3; 4]);

  (* Test case: Array contains duplicate elements *)
  assert (closest_minimum_elements_sum [2; 3; 2; 3; 4] 8 = [2; 2; 4]);

  (* Test case: Target is equal to one of the elements *)
  assert (closest_minimum_elements_sum [5; 10; 15] 10 = [10]);

  (* Test case: Array contains both positive and negative numbers *)
  assert (closest_minimum_elements_sum [-2; -1; 1; 2; 3] 1 = [-1; 2])