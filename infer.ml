open Unification
open Subs
open Ast
open Printf


type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr

let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  let report t1 t2 =
    Error (Printf.sprintf "cannot unify %s and %s" (string_of_texpr t1) (string_of_texpr t2)) in
  match e with
  | Unit -> OK (n, (create (), e, UnitType))
  | Int x -> OK (n, (create (), e, IntType))
  | Var s -> OK (n+1,
    let tv = "v" ^ string_of_int n in
    let tc = create () in
    extend tc s @@ VarType (tv);
    (tc, e, VarType (tv))
  )
  | Add (e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) ->
    (match infer' e1 n with
      | OK (n1, (s1, _, t1)) ->
        (match infer' e2 n1 with
        | OK (n2, (s2, _, t2)) -> 
          (* compat(s1, s2) *)
          (match mgu [(t1, IntType) ; (t2, IntType)] with
          | UOk s ->
              if compat s1 s2
              then OK (n2, (join [s1;s2;s], e, IntType))
              else Error (sprintf "compat(%s, %s) failed" (string_of_subs s1) (string_of_subs s2))
          | UError (t1, t2) -> report t1 t2)
        | Error s -> Error s)
      | Error s -> Error s)
  | IsZero e1 ->
    (match infer' e1 n with
    | OK (n1, (s1, _, t1)) ->
      (match mgu [(t1, IntType)] with
      | UOk s -> OK (n1, (join [s1;s], e, BoolType))
      | UError (t1, t2) -> report t1 t2)
    | Error s -> Error s)
  | App (f, x) ->
    (match infer' f n with
    | OK (n1, (s1, _, t1)) ->
      (match infer' x n1 with
      | OK (n2, (s2, _, t2)) ->
        let ret = VarType ("v" ^ string_of_int (n2)) in
        (match mgu [(t1, FuncType (t2, ret))] with
        | UOk s ->
            if compat s1 s2
            then OK (n2+2, (join [s1;s2;s], e, apply_to_texpr s ret))
            else Error (sprintf "compat(%s, %s) failed" (string_of_subs s1) (string_of_subs s2))
        | UError (t1, t2) -> report t1 t2)
      | Error s -> Error s)
    | Error s -> Error s)
  | _ -> failwith @@ "infer': undefined for " ^ string_of_expr e



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
