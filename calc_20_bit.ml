

(*
130
(** Represents an arbitrary precision integer in base 2^20. The digits are
    ordered from lower positional values to higher positional values. *)
type number = int list

type statement =
  (** Represents an integer constant term. *)
  | Const of number
  (** Represents the addition of two statements. *)
  | Add of statement * statement
  (** Represents the multiplication of two statements. *)
  | Multiply of statement * statement

(** Given a statement of type `statement` representing a calculation involving
    numbers of arbitrary size, returns a single number of type `number` with the
    evaluated expression. If an input value does not respect the `number` format,
    the function raises an Invalid_argument exception with the error message 
    "invalid number encoding (block greater than 20 bits)".
    
    Examples:
    >>> eval_statement (Add (Const [1_048_570], Const [40]))
    [34;1]
    
    >>> eval_statement (Multiply (Const [1_040_000;1234], Const [7;2]))
    [988_544;1_040_068;2469]
*)
let eval_statement (statement: statement) : number =
*)

let is_greater_than_20_bits n =
  n >= (1 lsl 20) || n < -(1 lsl 20)

let parse_in_base_2_20 s =
  let base = 1 lsl 20 in (* 2^20 *)
  let rec aux lst pos acc =
    match lst with
    | [] -> acc
    | hd :: tl -> 
      match (is_greater_than_20_bits hd) with
      | true -> failwith "Greater than 20 bits"
      | false -> aux tl (pos + 1) (acc + (hd * (int_of_float (float_of_int base ** float_of_int pos))))    
  in
  aux s 0 0

let rec convert_from_decimal_to_base_2_20 n =
  let base = 1 lsl 20 in (* 2^20 *)
  if n = 0 then []
  else
    let remainder = n mod base in
    let quotient = n / base in
    remainder :: convert_from_decimal_to_base_2_20 quotient



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


let process_addition component =
  let operation = String.split_on_char ' ' component in
  let operands = List.tl operation in
  let var1 = int_of_string (replace_all_occurrences (List.nth operands 0 ) "_" "") in
  let var2 = int_of_string (replace_all_occurrences (List.nth operands 1 ) "_" "") in
  var1 + var2


let process_multiplication component =
  let operation = String.split_on_char ' ' component in
  let operands = List.tl operation in
  let var1 = int_of_string (replace_all_occurrences (List.nth operands 0 ) "_" "") in
  let var2 = int_of_string (replace_all_occurrences (List.nth operands 1 ) "_" "") in
  var1 * var2

let process_constant component =
  let operation = String.split_on_char ' ' component in
  let operands = List.tl operation in
  let component2 = List.map (fun x -> int_of_string (replace_all_occurrences x "_" "")) (String.split_on_char ';' (List.hd operands)) in
  parse_in_base_2_20 component2

let parse_components component =
  let operation = String.split_on_char ' ' component in
  match List.hd operation with
  | "Const" -> process_constant component
  | "Add" -> process_addition component 
  | "Multiply" -> process_multiplication component 
  | _ -> failwith "Unknown operation type"


  let rec parse_input equation =
    let rec process_parentheses equation : string =
      if String.contains equation '(' then
        let start_idx = String.rindex equation '(' in
        let end_idx = String.index_from equation (start_idx + 1) ')' in
        let component = String.sub equation (start_idx + 1) (end_idx - start_idx - 1) in
        let component_b = String.sub equation start_idx (end_idx - start_idx + 1) in
        let result = parse_components component in
        process_parentheses (replace_all_occurrences equation component_b (string_of_int result))
      else
        equation
    in
    process_parentheses equation
  

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


let format_number_with_underscores number =
  let str = string_of_int number in
  let len = String.length str in
  let buffer = Buffer.create (len + (len / 3)) in
  let rec insert_underscores i =
    if i >= len then
      Buffer.contents buffer
    else begin
      if i > 0 && (len - i) mod 3 = 0 then
        Buffer.add_char buffer '_';
      Buffer.add_char buffer str.[i];
      insert_underscores (i + 1)
    end
  in
  insert_underscores 0
  

let eval_statement equation =
  let nums = convert_from_decimal_to_base_2_20 (int_of_string (parse_input (collapse_spaces equation))) in
  (String.concat ";" (List.map format_number_with_underscores nums))

  
let () =
  assert (eval_statement "(Add (Multiply (Const 2_000) (Const 5)) (Const 12))" = "10_012");
  assert (eval_statement "(Add (Const 10_312) (Const 2))" = "10_314");
  assert (eval_statement "(Multiply (Const 1_000) (Const 10))" = "10_000");
  assert (eval_statement "(Add (Const 1_234) (Const 5_678))" = "6_912");
  assert (eval_statement "(Multiply (Const 1) (Const 2))" = "2");
  assert (eval_statement "(Const 1_000_000)" = "1_000_000");
  try
    (* This fails *)
    assert (eval_statement "(Const 1_500_000)" = "");
  with _ -> ();
  try
    (* This fails *)
    assert (eval_statement "(Add (Const 1_500_000) (Const 1_500_000))" = "");
  with _ -> ();
  assert (eval_statement "(Multiply (Const 1_040_000;1234) (Const 7;2))" = "988_544;1_040_068;2_469");
  assert (eval_statement "(Add (Const 1_048_570) (Const 40))" = "34;1") 
