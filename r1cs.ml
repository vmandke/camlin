(* 

129
"type statement =
  (** Represents an integer constant term. *)
  | Const of int
  (** Represents a term as base * power. The power is in the range [1..10]. *)
  | Term of string * int
  (** Represents the addition of two statements. *)
  | Add of statement * statement
  (** Represents the multiplication of two statements. *)
  | Multiply of statement * statement

type variable =
  (** Encodes the integer value `1`. *)
  | One
  (** A variable such as `x` or `y`. *)
  | Var of string
  (** The output variable (the value of the statement). *)
  | Out
  (** Intermediate (anonymous) variable. *)
  | Intermediate

(** A gate represents a single step in the computation of the QAP. The gate value 
    is calculated as `s . a * s . b`. In this equation, `s` is the vector of the 
    current values of the gates (gates not yet calculated are zero) and `a`, `b` 
    are weights to be applied.
    
    For a problem with `n` variables and `m` intermediate values, the dimension of 
    the vectors should be `n + m + 2` because of the extra `One`, and `Out` entries.
    
    The vector `c` should contain `0` for all entries except for the entry 
    corresponding to the current gate in `s`.
    
    Once the current gate's value is set in `s`, we should thus have:
    
    `s . a * s . b - s . c = 0`
*)
type gate = {
  a: int list;
  b: int list;
  c: int list;
}

type r1cs = {
  (** The variable names of the R1CS. The first variable should always be `One`,
      followed by the statement variables such as (`Var "x"`, `Var "y"`), and 
      then `Out` and then some `Intermediate` values if any. *)
  variables: variable list;
  (** List of gates in processing order to compute `Intermediate` values, and 
      finally the `Out` value. For a problem with `n` intermediate values, there 
      should be `n + 1` gates. *)
  gates: gate list;
}

(** Given a statement of type `statement`, returns the equivalent quadratic arithmetic 
    program encoded in a Rank 1 Constraint System of type `r1cs`. The gates of the 
    `r1cs` should favor low values for vector `b` and compute as much as possible in 
    the "early" gates. For example, to compute `2x^2`, the gates should correspond to 
    computing the intermediate value `2x` in step 1, then multiply this value by `x` 
    in the next step. The solution should minimize the number of intermediate states.
    
    References:
    * [R1CS](https://tlu.tarilabs.com/cryptography/rank-1).
    * [QAP from Zero to Hero](https://medium.com/@VitalikButerin/quadratic-arithmetic-programs-from-zero-to-hero-f6d558cea649)
    
    Polynomial `5`
    >>> r1cs (Const 5)
    {
      variables: [One; Out];
      gates: [
        { a = [5;0]; b = [1;0]; c = [0;1] };
      ]
    }
    
    Polynomial `x - 1`
    >>> r1cs (Add (Var "x", (Const -1)))
    {
      variables: [One; Var "x"; Out];
      gates: [
        { a = [-1;1;0]; b = [1;0;0]; c = [0;0;1] };
      ]
    }
    
    Polynomial `2xy + 5`
    >>> r1cs (Add (Multiply (Multiply (Const 2, Var "x"), Var "y"), (Const 5)))
    {
      variables: [One; Var "x"; Var "y"; Out; Intermediate];
      gates: [
        { a = [0;2;0;0;0]; b = [0;0;1;0;0]; c = [0;0;0;0;1] };
        { a = [5;0;0;0;1]; b = [1;0;0;0;0]; c = [0;0;0;1;0] };
      ]
    }
*)
let to_r1cs (statement: statement) : r1cs =" *)

type decomposition_node = {
  operation: string;
  mutable mul_vars: string list;
  mutable add_vars: (string * int) list;
  mutable constant: string;
  var_value: string;
}


type gate = {
  a: int list;
  b: int list;
  c: int list;
}




(* let print_matrix matrix =
  Array.iter (fun row ->
    Array.iter (fun elem ->
      print_int elem;      (* Print each element *)
      print_string " "     (* Separate elements with a space *)
    ) row;
    print_newline ()        (* Move to the next line after each row *)
  ) matrix
   
  let pretty_print_decomposition decomposition =
  Hashtbl.iter (fun key node ->
    Printf.printf "Node: %s\n" key;
    Printf.printf "  Operation: %s\n" node.operation;
    Printf.printf "  Mul Vars: [%s]\n" (String.concat "; " node.mul_vars);
    Printf.printf "  Add Vars: [%s]\n" (String.concat "; " (List.map (fun (v, c) -> Printf.sprintf "(%s, %d)" v c) node.add_vars));
    Printf.printf "  Constant: %s\n" node.constant;
    Printf.printf "  Var Value: %s\n" node.var_value;
    Printf.printf "\n"
  ) decomposition *)
  


let create_node operation mul_vars add_vars constant var_value =
  { operation; mul_vars; add_vars; constant; var_value }

let is_mul node = node.operation = "Multiply"
let is_add node = node.operation = "Add"
let is_var node = node.operation = "Var"

let string_of_node node =
  if is_var node then
    Printf.sprintf "DNode(Var::%s)" node.var_value
  else if is_mul node then
    Printf.sprintf "DNode(MUL :: %s :: %s)" (String.concat ", " node.mul_vars) node.constant
  else if is_add node then
    Printf.sprintf "DNode(ADD :: %s :: %s)" 
      (String.concat ", " (List.map (fun (v, c) -> Printf.sprintf "(%s, %d)" v c) node.add_vars))
      node.constant
  else
    "Unknown node type"

let remove_prefix prefix str =
  let prefix_length = String.length prefix in
  if String.length str >= prefix_length && String.sub str 0 prefix_length = prefix then
    String.sub str prefix_length (String.length str - prefix_length)
  else
    str

let mul_all_lists l =
  List.fold_left (fun acc x -> 
    acc * int_of_string (remove_prefix "C_" x)
  ) 1 l

let add_all_lists l =
  List.fold_left (fun acc x -> 
    acc + int_of_string (remove_prefix "C_" x)
  ) 0 l

  let process_addition component decomposition =
    let operation = String.split_on_char ' ' component in
    let operands = List.tl operation in
    let constants = List.filter (fun c -> String.starts_with ~prefix:"C_" c) operands in
    let vars = List.filter (fun c -> not (String.starts_with ~prefix:"C_" c)) operands in
    let vars = List.map (fun v ->
      if String.contains v '*' then
        let parts = String.split_on_char '*' v in
        (List.hd parts, mul_all_lists (List.tl parts))
      else (v, 1)
    ) vars in
    let add_params = List.fold_left (fun acc (v, count) ->
      if List.mem_assoc v acc then
        (v, (List.assoc v acc) + count) :: (List.remove_assoc v acc)
      else (v, count) :: acc
    ) [] vars in
    let var_name = "v" ^ (string_of_int (Hashtbl.length decomposition)) in
    let constant = "C_" ^ (string_of_int (add_all_lists constants)) in
    Hashtbl.add decomposition var_name (create_node "Add" [] add_params constant "");
    var_name

let process_multiplication component decomposition =
  let operation = String.split_on_char ' ' component in
  let operands = List.flatten (List.map (String.split_on_char '*') (List.tl operation)) in
  let constants, vars = List.partition (fun c -> String.starts_with ~prefix:"C_" c) operands in
  let constant = "C_" ^ (string_of_int (mul_all_lists constants)) in
  let prev_decomposition = Hashtbl.fold (fun k v acc ->
    if is_mul v && v.mul_vars = vars then k :: acc else acc
  ) decomposition [] in
  match prev_decomposition, vars with
  | h::_, _ -> h
  | [], [v] -> String.concat "*" (vars @ [constant])
  | [], _ ->
      let var_name = "v" ^ (string_of_int (Hashtbl.length decomposition)) in
      Hashtbl.add decomposition var_name (create_node "Multiply" vars [] constant "");
      var_name

let parse_components component decomposition =
  let operation = String.split_on_char ' ' component in
  match List.hd operation with
  | "Const" -> "C_" ^ (List.nth operation 1)
  | "Add" -> process_addition component decomposition
  | "Multiply" -> process_multiplication component decomposition
  | _ when String.starts_with ~prefix:"Var" component ->
      let var_value = List.nth operation 1 in
      Hashtbl.add decomposition var_value (create_node "Var" [] [] "" var_value);
      var_value
  | _ -> failwith "Unknown operation type"


let has_foldable_addition decomposition =
  Hashtbl.fold (fun _ node acc -> acc || (is_add node && List.exists (fun (v, _) -> is_add (Hashtbl.find decomposition v)) node.add_vars)) decomposition false

let merge_addition decomposition parent_key child_key child_count =
  let parent = Hashtbl.find decomposition parent_key in
  let child = Hashtbl.find decomposition child_key in
  parent.add_vars <- List.fold_left (fun acc (v, c) ->
    if v = child_key then acc else (v, c) :: acc
  ) [] parent.add_vars;
  parent.add_vars <- List.fold_left (fun acc (v, c) ->
    if List.mem_assoc v acc then
      (v, (List.assoc v acc) + (c * child_count)) :: (List.remove_assoc v acc)
    else (v, c * child_count) :: acc
  ) parent.add_vars child.add_vars;
  parent.constant <- "C_" ^ (string_of_int ((int_of_string (remove_prefix "C_" parent.constant)) + ((int_of_string (remove_prefix "C_" child.constant)) * child_count)));
  Hashtbl.remove decomposition child_key


let replace_all_occurrences s s1 s2 =
  let len_s = String.length s in
  let len_s1 = String.length s1 in
  let buf = Buffer.create len_s in
  let rec aux i =
    if i >= len_s then
      Buffer.contents buf
    else if i <= len_s - len_s1 && String.sub s i len_s1 = s1 then
      (Buffer.add_string buf s2;
        aux (i + len_s1))
    else
      (Buffer.add_char buf s.[i];
        aux (i + 1))
  in
  aux 0


let rec parse_input equation decomposition =
  let rec process_parentheses equation =
    if String.contains equation '(' then
      let start_idx = String.rindex equation '(' in
      let end_idx = String.index_from equation (start_idx + 1) ')' in
      let component = String.sub equation (start_idx + 1) (end_idx - start_idx - 1) in
      let component_b = String.sub equation start_idx (end_idx - start_idx +1) in
      let var_name = parse_components component decomposition in
      let new_equation = replace_all_occurrences equation component_b var_name in
      process_parentheses new_equation
    else
      equation
  in
  let equation = process_parentheses equation in
  Hashtbl.add decomposition "Out" (Hashtbl.find decomposition equation);
  Hashtbl.remove decomposition equation;
  while has_foldable_addition decomposition do
    let fold_vars = List.filter (fun (v, _) -> is_add (Hashtbl.find decomposition v)) (Hashtbl.find decomposition "Out").add_vars in
    List.iter (fun (var_2_fold, var_count) -> merge_addition decomposition "Out" var_2_fold var_count) fold_vars;
  done;
  let variables = "One" :: (Hashtbl.fold (fun var node acc -> if is_var node then var :: acc else acc) decomposition []) @ ["Out"] in
  let intermediate_keys = Hashtbl.fold (fun var _ acc -> if List.mem var variables then acc else var :: acc) decomposition [] in
  (variables, intermediate_keys)
  


  let rec find_index x lst =
    match lst with
    | [] -> raise (Failure "Not Found in Index!!")
    | h :: t -> if x = h then 0 else 1 + find_index x t

  
  let get_abc variables intermediate_keys decomposition =
    let intermediate_steps = List.length intermediate_keys in
    let witness = variables @ (
      match intermediate_steps with
      | 0 -> []
      | _ -> (List.init (intermediate_steps - 1) (fun i -> List.nth intermediate_keys i))
    ) in
    let total_equations = max intermediate_steps 1 in
    let len_witness = List.length witness in
    let a_gate = Array.make_matrix total_equations len_witness 0 in
    let b_gate = Array.make_matrix total_equations len_witness 0 in
    let c_gate = Array.make_matrix total_equations len_witness 0 in

    
    
    (* Update A, B, C for each equation *)
    for i = 0 to total_equations - 2 do
      let constraint_name = List.nth intermediate_keys i in
      match Hashtbl.find decomposition constraint_name with
      | { operation = "Multiply"; mul_vars = [ai; bi]; constant; _ } ->
          let constant_value = int_of_string (remove_prefix "C_" constant) in
          a_gate.(i).(find_index ai witness) <- constant_value;
          b_gate.(i).(find_index bi witness) <- 1;
          c_gate.(i).(find_index constraint_name witness) <- 1
      | _ -> failwith "Expected Multiply operation"
    done;
  
    (* Update last equation *)
    let i = total_equations - 1 in
    let add_vars = (Hashtbl.find decomposition "Out").add_vars in
    let non_witness = (List.filter (fun (v, _) -> not (List.mem v witness)) add_vars) in
    let (rhs_var, rhs_count) = match List.length non_witness with
    | 0 -> List.nth add_vars 0;
    | _ -> List.nth non_witness 0 in
    let lhs = List.map (fun (v, c) -> (v, -c)) (List.filter (fun (v, _) -> List.mem v witness && v != rhs_var) add_vars) @ [("Out", 1)] in
    List.iter (fun (v, c) -> c_gate.(i).(find_index v witness) <- c) lhs;
    let rhs_node = Hashtbl.find decomposition rhs_var in
    match Hashtbl.find decomposition rhs_var with
      | { operation = "Multiply" } ->
        begin
          let rhs_constant = (int_of_string (remove_prefix "C_" rhs_node.constant)) * rhs_count in
          a_gate.(i).(find_index (List.nth rhs_node.mul_vars 0) witness) <- rhs_constant;
          b_gate.(i).(find_index (List.nth rhs_node.mul_vars 1) witness) <- 1;
        end
      | { operation = "Var" } ->
        a_gate.(i).(find_index rhs_node.var_value witness) <- 1;
      | _ -> ();
    ;
    witness, a_gate, b_gate, c_gate
  

let collapse_spaces str =
  let len = String.length str in
  let rec collapse acc idx =
    if idx >= len then
      acc
    else
      let current_char = str.[idx] in
      if current_char = ' ' && (idx = 0 || str.[idx - 1] = ' ') then
        collapse acc (idx + 1)
      else
        collapse (acc ^ String.make 1 current_char) (idx + 1)
  in
  collapse "" 0


let get_r1cs equation =
  let decomposition = Hashtbl.create 10 in
  let variables, intermediate_keys = parse_input (collapse_spaces equation) decomposition in
  (* pretty_print_decomposition decomposition; *)
  get_abc variables intermediate_keys decomposition


 
  
let () =
  let equation1 = "(Add (Multiply (Multiply (Const 2) (Var x)) (Var y)) (Const 5))" in
  let witness, a_gate, b_gate, c_gate = get_r1cs equation1 in
  assert (["One"; "x"; "y"; "Out"] = witness );
  assert (a_gate = [| [| 0; 2; 0; 0 |] |]);
  assert (b_gate = [| [| 0; 0; 1; 0 |] |]);
  assert (c_gate = [| [| 0; 0; 0; 1 |] |]);

  let equation2 = "(Add (Add (Add (Multiply (Multiply (Multiply (Const 3)    (Var x)) (Var x)) (Var y)) (Multiply (Multiply (Const 5) (Var x)) (Var y))) (Multiply (Const -1) (Var x))) (Const 3))" in
  let witness, a_gate, b_gate, c_gate = get_r1cs equation2 in
  assert (["One"; "x"; "y"; "Out"; "v2"; "v3"] = witness);
  assert (a_gate = [|
    [| 0; 5; 0; 0; 0; 0 |];
    [| 0; 3; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0; 1 |]
  |]);
  assert (b_gate = [|
      [| 0; 0; 1; 0; 0; 0 |];
      [| 0; 1; 0; 0; 0; 0 |];
      [| 0; 0; 1; 0; 0; 0 |]
    |]
  );
  assert (
    c_gate = [|
      [| 0; 0; 0; 0; 1; 0 |];
      [| 0; 0; 0; 0; 0; 1 |];
      [| 0; 1; 0; 1; -1; 0 |]
    |]
  );

  let equation3 = "(Add (Var x) (Const -1))" in
  let witness, a_gate, b_gate, c_gate = get_r1cs equation3 in
  assert (a_gate = [|[| 0; 1; 0 |]|] );
  assert (["One"; "x"; "Out";] = witness);

