(* "def marathonComplete(d, p):
    """"""
    Determines the minimum value of k such that the distance d km can be completed,
    given that after each rest, the distance Alice can run is reduced by a factor of p.
    
    Args:
    - d (int): The distance that needs to be run to complete the marathon.
    - p (int): The factor by which the distance Alice can run is reduced after each rest.

    Returns:
    - int: The minimum value of k such that the marathon is completed.

    Example:
    marathonComplete(5, 2) => 4

    Explanation:
    If k = 4 and p = 2:
    Alice first runs 4 km, then rests.
    She then runs 4 // 2 = 2 km. Now, 4 + 2 = 6 km, which is greater than 5 km (the marathon distance).
    Thus, Alice completes the marathon. For any value of k < 4, Alice cannot complete the marathon.
    Hence, the minimum k for Alice to choose is 4.
    """"""" *)




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