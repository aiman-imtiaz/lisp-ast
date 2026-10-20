open Code
open Lexer
open Parser
open Evaluator

(* Execute infinitely until EOF is triggered *)
let () = 
  let in_channel = stdin in
  try
    Printf.printf("> %!") ;
    let tokens = lex in_channel in
    (* print_tokens tokens; *)
    let ast = parse tokens in
    (* print_ast ast; *)
    print_ast (eval ast)
  with
  | Lex_error c -> Printf.printf "Char %s is unexpected.\n" (Char.escaped c)



