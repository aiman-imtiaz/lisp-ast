(*  Parser.ml 
* 
*)
open Lib
open Ast
open Str

exception ParseError of string;;

let print_ast ast = printAst ast

let get_atom token = 
  try
    if Str.string_match (Str.regexp "^[0-9]+") token 0 
    then Atom (Literal (CInt (int_of_string token)))
    else if Str.string_match (Str.regexp "\"") token 0 
    then Atom (Literal (CStr token))
    else if String.equal "t" token then Atom(Literal (CBool true))
    else if String.equal "f" token then Atom(Literal (CBool false))
    else Atom(Symbol token)
  with
  | Failure s -> raise (ParseError "invalid number")

let parse(tokens:string list) =
  let rec build_ast (tokens: string list) (root:ast) =
    match tokens with
    | [] -> root, []
    | h::tail -> 
      match h with
      | "(" -> 
        let exp, rest = build_ast tail (Form []) in
        let next, restOfRest = build_ast rest root in
        appendToForm exp next, restOfRest
      | ")" -> root, tail
      | x -> let next, rest = build_ast tail root in
              appendToForm (get_atom x) next, rest
  in
  match tokens with
  | [] -> Empty
  | "(" :: tail -> let ast, _ = build_ast tail (Form []) in ast
  | h :: [] -> get_atom h
  | h1::h2::tail -> raise (ParseError "cannot have multiple elements without brackets")
