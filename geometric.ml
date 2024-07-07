
(* Gemotric Progression: Ideal scenario
such first such K =>
D >= K(1 - (1/p))

*)

let marathon_complete d p =
  let rec loop k =
    let k_float = float_of_int k in
    let p_float = float_of_int p in
    if k_float /. (1.0 -. (1.0 /. p_float)) <= d then
      loop (k + 1)
    else
      k
  in
  loop 1

let () =
  assert (marathon_complete 5.0 2 = 3);
  assert (marathon_complete 5.0 3 = 4);
  assert (marathon_complete 5.0 1 = 1);