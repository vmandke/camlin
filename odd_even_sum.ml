

(* 
ABS(O-E) = X
Divide arr2 into a, b such that a = b + x
a + b = sum(arr2) => 2a = sum(arr2) + X
=> sum(arr2) + X  *)


let sum_indices arr mod_val =
  let sum = ref 0 in
  for i = 0 to Array.length arr - 1 do
    if mod_val = -1 || i mod 2 = mod_val then
      sum := !sum + arr.(i)
  done;
  !sum

let is_equal_sum_possible arr1 arr2 = 
  let odd_sum = sum_indices arr1 1 in
  let even_sum = sum_indices arr1 0 in
  let x = abs (odd_sum - even_sum) in
  let sum_arr2 = sum_indices arr2 (-1) in
  (sum_arr2 + x) mod 2 = 0


let () =
assert (is_equal_sum_possible [|5; 1; 3; 4|] [|1; 2; 3; 4|] = false);
assert (is_equal_sum_possible [|1;2;3;4;5|] [|3;1|] = false);
assert (is_equal_sum_possible [|3;2;6|] [|7|] = true);

