open Str

(* Run basic mathmatical operations *)
let do_math operator first_operand second_operand = 
  match operator with
    | "+" -> Ok (first_operand +. second_operand)
    | "-" -> Ok (first_operand -. second_operand)
    | "*" -> Ok (first_operand *. second_operand)
    | "/" -> 
        if (second_operand = 0.) then
          Error ("Divide by zero")
        else 
          Ok (first_operand /. second_operand)
    | "^" -> Ok (first_operand ** second_operand)
    | _ -> Error ("Invalid operator")
;;

(* Recrusively find the value or a reverse polish notation *)
let rec rpn input_list storage =
  match input_list with
    | [] -> 
      if Stack.length storage = 1 then
        Ok (Stack.pop storage)
      else 
        Error ("Invalid input")
    | hd -> 
      match List.hd input_list with
        | ("+"|"-"|"*"|"/"|"^") -> 
          begin
            if Stack.length storage >= 2 then
              match do_math (List.hd input_list) (Stack.pop storage) (Stack.pop storage) with
              | Ok output -> 
                Stack.push output storage;
                rpn (List.tl input_list) storage
              | Error output -> Error (output)
            else
              Error ("Not enough arguments")
          end
        | _ -> 
          match float_of_string_opt (List.hd input_list) with
          | Some output-> 
            Stack.push output storage;
            rpn (List.tl input_list) storage
          | None -> Error ("Not a number")
;;

(* Split an input string on spaces *)
let split_str raw_input =
  Str.split (Str.regexp " +") raw_input
;;

(* Print the output of rpn on a new line *)
let evaluate_line input = 
  match (rpn (split_str input) (Stack.create ())) with
    | Ok output -> string_of_float output
    | Error output -> output
;;

(* Get rpn from stdin and output result *)
print_endline (evaluate_line (read_line ()));;