let get_prefix_sum arr =
  let n = Array.length arr in
  let prefix_sum = Array.make (n + 1) 0 in
  for i = 0 to n - 1 do
    prefix_sum.(i + 1) <- prefix_sum.(i) + arr.(i)
  done;
  prefix_sum

let get_left_boundaries arr =
  let n = Array.length arr in
  let stack = Stack.create () in
  let left = Array.make n 0 in
  for i = 0 to n - 1 do
    while not (Stack.is_empty stack) && arr.(Stack.top stack) >= arr.(i) do
      ignore (Stack.pop stack)
    done;
    left.(i) <- (if Stack.is_empty stack then 0 else Stack.top stack + 1);
    Stack.push i stack
  done;
  left

let get_right_boundaries arr =
  let n = Array.length arr in
  let stack = Stack.create () in
  let right = Array.make n (n - 1) in
  for i = n - 1 downto 0 do
    while not (Stack.is_empty stack) && arr.(Stack.top stack) >= arr.(i) do
      ignore (Stack.pop stack)
    done;
    right.(i) <- (if Stack.is_empty stack then n - 1 else Stack.top stack - 1);
    Stack.push i stack
  done;
  right

let maximize_product nums =
  let left = get_left_boundaries nums in
  let right = get_right_boundaries nums in
  let prefix_sum = get_prefix_sum nums in
  let max_product = ref 0 in
  for i = 0 to Array.length nums - 1 do
    let subarray_sum = prefix_sum.(right.(i) + 1) - prefix_sum.(left.(i)) in
    max_product := max !max_product (subarray_sum * nums.(i))
  done;
  !max_product

let () =
assert (maximize_product [|3; 1; 6; 4; 5; 2|] = 60);
assert (maximize_product [|-1;2;-3;4|] = 16);
assert (maximize_product [|-4;1;5;4;2;5|] = 36);
