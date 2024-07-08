(* Goal of the problem statement - 
1. Given 2 arrays with positive integers.
2. Pick up all the elements in 2nd array one by one, and add them to any of the element of first array.
3. Is it possible to make sum of elements at odd indexes equals to sum of elements at even indexes in the resultant first array. Return true or false.

Input - 
Size of both arrays < 100
Only +ve integers in both of the arrays.
0<elements<10^4 


Rough Solution - 
1. Lets consider initial sum of odd = O, and even = E. absolute difference can be X, |E-O|=X.
2. Total sum of elements of 2nd array can be T.
3. Divide elements in second array in two groups, One group will be added to odd indexes and another to even indexes. Sum of first group is A and other is B. We know that B = T-A.
4. If we take a deeper look, O+A = E+B (we want this condition to arrive.)
5.  Solve the maths (let's consider E is greate than O, it doesn't matter though)-
a. A = B + E- O 
b. A=B+X
c. A = T - A + X
d. 2A = T+X
e. A = (T+X)/2

Now, T and X are known numbers. This basically boils down to the fact that, can we find a group(subset) of numbers in second array, such that it's sum is equal to A.(a known number)

Specific case - 
If T+X is an odd number a solution isn't possible. this should be checked.
Eg - 
1st array - 1 2 3 4 5
2nd array - 3 1
Ans - false *)


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