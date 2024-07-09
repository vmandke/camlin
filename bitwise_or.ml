(* 
117
def maximize_bitwise_or(nums: List[int], moves: int) -> int:
    """"""
    Given a list of integers and a number of moves, maximize the bitwise OR of the integers
    by incrementing any integer by 1 in each move.

    Args:
        nums (List[int]): A list of integers.
        moves (int): The number of allowed moves.

    Returns:
        int: The maximum bitwise OR value that can be obtained.

    Examples:
        >>> maximize_bitwise_or([1, 2, 3], 2)
        7
        >>> maximize_bitwise_or([5, 1], 3)
        7 
*)



let get_bitwise_or arr =
  Array.fold_left (fun acc x -> acc lor x) 0 arr

let slice_and_concat nums i =
  let left_slice = Array.sub nums 0 i in
  let right_slice = Array.sub nums (i + 1) (Array.length nums - i - 1) in
  Array.append left_slice right_slice

let maximize_bitwise_or nums max_moves =
  let arr = Array.of_list nums in
  let moves = ref max_moves in
  Array.sort (fun a b -> compare b a) arr;
  for i = 0 to Array.length arr - 1 do
    if !moves > 0 then
      let other_arr = slice_and_concat arr i in
      let other_or = get_bitwise_or other_arr in
      let current_max_bitwise_or = other_or lor arr.(i) + !moves in
      let pending_moves = ref 0 in
      while current_max_bitwise_or = other_or lor arr.(i) + !moves - !pending_moves - 1 do
        pending_moves := !pending_moves + 1
      done;
      arr.(i) <- arr.(i) + !moves - !pending_moves;
      moves := !pending_moves
  done;
  get_bitwise_or arr

(* Example usage *)
let () =
  assert (maximize_bitwise_or [5; 1] 3 = 9);
  assert (maximize_bitwise_or [1; 2; 3] 2 = 7);
  assert (maximize_bitwise_or [15; 15] 1 = 31);
  assert (maximize_bitwise_or [4; 5; 6] 1 = 7); (* Best OR value is 4 | 5 | 7 = 7 *)
  assert (maximize_bitwise_or [10; 20; 30] 5 = 63); (* Best OR value with moves should give 63 *)
  assert (maximize_bitwise_or [7; 8; 9] 0 = 15); (* No moves, just OR the values *)
  assert (maximize_bitwise_or [1; 1; 1; 1] 3 = 5); (* Incrementing values should maximize OR *)
  assert (maximize_bitwise_or [1; 1; 1; 1; 1; 1; 1; 1; 1; 1] 10 = 11); (* Plenty of moves to maximize OR *)
  assert (maximize_bitwise_or [5; 10; 15; 20] 2 = 31); (* Mixed values, limited moves *)