(* Ast.ml
*
*
*)
exception AstError of string;;


type const = 
| CInt of int
| CStr of string
| CBool of bool
(* | Nil
| List of const * const *)

and value = const
and token =
  | Literal of value
  | Symbol of string

type ast = 
| Atom of token
| Form of ast list
| Empty
  
let appendToForm elem form =
  match form with
  | Form lst -> Form (elem :: lst)
  | _ -> raise (AstError "Tried to append to Atom")

let printToken token =
    match token with
    | Literal value ->
      begin
        match value with
        | CInt x -> Printf.printf("%d %!") x;
        | CStr s -> Printf.printf("%s %!") s;
        | CBool b -> if b then Printf.printf("true %!") else Printf.printf("false %!")
      end
    | Symbol value -> Printf.printf("%s %!") value

let rec printAst ast =
  match ast with
  | Empty -> () (* do nothing *)
  | Atom token -> printToken token
  | Form lst ->
    begin
      Printf.printf("[ %!") ;
      List.iter printAst lst;
      Printf.printf("] %!")
    end