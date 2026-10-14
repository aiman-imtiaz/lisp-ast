(*  Lexer.ml 
 * 
 *)


let rec print_tokens (tokens: string list): unit =
  match tokens with
  | [] -> Printf.printf("\n%!")
  | t::tail -> Printf.printf("%s %!") t;
                print_tokens(tail)

(* This exception is raised when an unanticipated character is read *)
exception Lex_error of char

let rec lex (input: in_channel) :  string list =
  let acc = ref [] in
  let emit t = acc := t::(!acc) in

  let rec next (c:char) =
    match c with
    | '(' | ')' -> (let t = String.make 1 c in emit t; (*Printf.printf("%s %!") t; *) next (input_char input))
    | '\t'| ' ' | '\n' | '\r' -> next (input_char input)
    | '*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+' -> (emit (String.make 1 c); next (input_char input))
    | 'A'..'Z' | 'a'..'z' -> lex_word c (* keyword or variable name MUST start with letter*)
    | '"' -> lex_string c
    | '0'..'9' -> lex_number c
    | _ -> raise (Lex_error c)
    
  and lex_number (c:char) =
    let buf = Buffer.create 16 in
    let rec loop () =
      let next_c = input_char input in
      match next_c with
      | '0'..'9' -> (Buffer.add_char buf next_c;
	          loop ())
	    | _ -> (emit (Buffer.contents buf);
	          next next_c)
    in
    Buffer.add_char buf c;
    loop ()
  
  and lex_word (c:char) =
    let buf = Buffer.create 16 in
    let rec loop () =
      let next_c = input_char input in
      match next_c with
      | 'A'..'Z' | 'a'..'z' | '-'  -> (Buffer.add_char buf next_c;
	          loop ())
	    | _ -> (emit (Buffer.contents buf);
	          next next_c)
    in
    Buffer.add_char buf c;
    loop ()
  
  and lex_string (c:char) =
    let buf = Buffer.create 16 in
    let rec loop () =
      let next_c = input_char input in
      match next_c with
	    | '"' -> (Buffer.add_char buf next_c;
                emit (Buffer.contents buf);
	              next (input_char input))
      | _ -> (Buffer.add_char buf next_c;
	          loop ())
    in
    Buffer.add_char buf c;
    loop ()

  and exit () = 
    List.rev (!acc) 

  in
    try
      next (input_char input) 
    with
      | End_of_file -> exit ()


