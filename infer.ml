open Unification
open Subs
open Ast


type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr


let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  match e with
  | Int x -> OK (n, (create (), e, IntType))
  | Var s -> OK (n+1,
    let tv = string_of_int (n+1) in
    let tc = create () in
    extend tc s @@ VarType (tv);
    (tc, e, VarType (tv))
  )
  | _ -> failwith "infer': undefined"



let string_of_typing_judgement = function
| (tenv, expr, texpr) ->
  (string_of_subs tenv) ^ " ⊢ " ^ (string_of_expr expr) ^ " : " ^ (string_of_texpr texpr)


let infer_type (AProg e) =
  match infer' e 0 with
  | OK (_, tj) -> string_of_typing_judgement tj
  | Error s -> "Error! "^ s



let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


(* Interpret an expression *)
let inf (e:string) : string =
  e |> parse |> infer_type

let test (n:int) : string =
  Examples.expr n |> parse |> infer_type
