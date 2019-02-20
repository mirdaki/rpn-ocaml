(* Defintion for input items *)
type token =
  | Number of float
  | Operator of string
  | TokenError of string
;;

(* Convert a single string into a token *)
let to_token (input: string) : token =
  match input with
    | ("+"|"-"|"*"|"/"|"^") -> Operator(input)
    | _ -> let value = float_of_string_opt(input) in
      match value with
        | Some(num) -> Number(num)
        | None -> TokenError("Unrecognized token '" ^ input ^ "'")
;;

(* Run basic mathmatical operations *)
let do_math (operator: string) (operand1: float) (operand2: float) = 
  match operator with
    | "+" -> Ok(operand1 +. operand2)
    | "-" -> Ok(operand1 -. operand2)
    | "*" -> Ok(operand1 *. operand2)
    | "/" -> 
      if (operand2 = 0.) then
        Error("Divide by zero")
      else 
        Ok(operand1 /. operand2)
    | "^" -> Ok(operand1 ** operand2)
    | _ -> Error("Invalid operator")
;;

(* The results from the RPN evaluation *)
type stackState =
  | Stack of float list
  | StackError of string
;;

(* Do RPN evaluation for a single item and stack *)
let next_state (state: stackState) (index: token) : stackState =
  match state with
    | StackError(err) -> state
    | Stack(lst) ->
      match index with
        | Number(flt) -> Stack(flt::lst)
        | Operator(op) -> (
          match lst with
            | operand2::operand1::tail -> (
              match do_math op operand1 operand2 with 
                | Ok(num) -> Stack(num::tail)
                | Error(err) -> StackError(err)
            )
            | _ -> StackError("Not enough arguments for '" ^ op ^ "'")
          )
        | TokenError(err) -> StackError(err)
;;

(* Convert the final RPN output to a string *)
let result_to_string (input: stackState) : string = 
  match input with
    | Stack(lst) -> (
      match lst with
      | hd::[] -> string_of_float hd
      | _ -> "Not enough operators"
    )
    | StackError(output) -> output
;;

(* Connect all of the RPN functions together to do the RPN evaluation *)
let process_rpn (input: string) =
  input |> (* Get the input *)
  String.split_on_char ' ' |> (* Break up the input on spaces *)
  List.map to_token |> (* Make the broken up string into a token list *)
  List.fold_left next_state (Stack[]) |> (* Find the stack processing the input left-to-right *)
  result_to_string (* result into string *)
;;

(* Get rpn from stdin and output result *)
read_line () |> 
process_rpn |>
print_endline
;;