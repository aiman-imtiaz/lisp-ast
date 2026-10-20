(* Evaluator.ml
*
*)

open Lib
open Ast 

exception EvalError of string;;

let extract atom =
  match atom with
  | Atom(Literal(CInt x)) -> x
  | _ -> raise (EvalError "not supported yet")

let rec eval ast =
  match ast with
  | Empty -> ast
  | Form lst ->
    begin
    match lst with
    | Atom( Symbol "+") :: a :: b :: [] -> 
      let a1 = eval(a) in
      let b1 = eval (b) in
        Atom(Literal(CInt(extract(a1) + extract(b1))))
    | _ -> ast
    end
  | Atom a ->
    begin
      match a with
      | Literal (CInt x) -> ast
      | _ -> ast
    end