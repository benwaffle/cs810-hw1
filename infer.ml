open Unification
open Subs
open Ast
open Printf


type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr

let string_of_typing_judgement = function
  | (tenv, expr, texpr) ->
    (string_of_subs tenv) ^ " ⊢ " ^ (string_of_expr expr) ^ " : " ^ (string_of_texpr texpr)

let compat (xs : subst list) : (texpr * texpr) list =
  List.flatten @@ List.flatten @@ List.map (fun s1 ->
      List.map (fun s2 ->
          List.fold_left (fun acc var ->
              match lookup s2 var with
              | Some x -> (Hashtbl.find s1 var, x) :: acc
              | None -> acc
            ) [] (domain s1)
        ) xs
    ) xs

let apply_to_env2 s e =
  apply_to_env s e;
  e

let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  let report t1 t2 =
    Error (Printf.sprintf "cannot unify %s and %s" (string_of_texpr t1) (string_of_texpr t2)) in
  match e with
  | Unit -> OK (n, (create (), e, UnitType))
  | Int x -> OK (n, (create (), e, IntType))
  | Var s -> OK (n+1,
                 let tv = VarType("v" ^ string_of_int n) in
                 let tc = create () in
                 extend tc s tv;
                 (tc, e, tv)
                )
  | Add (e1, e2) | Sub(e1, e2) | Mul(e1, e2) | Div(e1, e2) ->
    (match infer' e1 n with
     | OK (n1, (s1, _, t1)) ->
       (match infer' e2 n1 with
        | OK (n2, (s2, _, t2)) -> 
          (match mgu @@ List.append [(t1, IntType);(t2, IntType)] (compat [s1;s2]) with
           | UOk s -> OK (n2, (join @@ List.map (apply_to_env2 s) [s1;s2], e, IntType))
           | UError (t1, t2) -> report t1 t2)
        | Error s -> Error s)
     | Error s -> Error s)
  | IsZero e1 ->
    (match infer' e1 n with
     | OK (n1, (s1, _, t1)) ->
       (match mgu [(t1, IntType)] with
        | UOk s ->
          apply_to_env s s1;
          OK (n1, (s1, e, BoolType))
        | UError (t1, t2) -> report t1 t2)
     | Error s -> Error s)
  | App (f, x) ->
    (match infer' f n with
     | OK (n1, (s1, _, t1)) ->
       (match infer' x n1 with
        | OK (n2, (s2, _, t2)) ->
          let ret = VarType ("v" ^ string_of_int (n2)) in
          (match mgu @@ (t1, FuncType (t2, ret)) :: compat [s1;s2] with
           | UOk s -> OK (n2+2, (join @@ List.map (apply_to_env2 s) [s1;s2], e, apply_to_texpr s ret))
           | UError (t1, t2) -> report t1 t2)
        | Error s -> Error s)
     | Error s -> Error s)
  | Proc (arg, argtype, body) ->
    (match infer' body n with
     | OK (n1, (s1, _, t1)) ->
       let arg = (match lookup s1 arg with
           | None -> VarType arg
           | Some t -> t) in
       (match mgu [(arg, argtype)] with
        | UOk s -> OK (n1, (apply_to_env2 s s1, e, apply_to_texpr s t1))
        | UError (t1, t2) -> report t1 t2)
     | Error s -> Error s)
  | ProcUntyped (arg, body) ->
    (match infer' body n with
     | OK (n1, (s1, e1, t1)) ->
       let arg_t = (match lookup s1 arg with
           | None -> VarType arg (* arg not used in body, make a VarType *)
           | Some t -> t) in (* arg used in body, get inferred type  *)
       let proc_typed = apply_to_expr (let ht = create() in extend ht arg arg_t; ht) @@ ProcUntyped(arg, e1) in (* convert ProcUntyped to Proc *)
       remove s1 arg; (* remove argument type from env because it's scoped *)
       OK (n1, (s1, proc_typed, FuncType (arg_t, t1)))
     | Error s -> Error s)
  | Let (var, exp, body) ->
    printf "let %s = %s in %s\n" var (string_of_expr exp) (string_of_expr body);
    (match infer' body n with
     | OK (n1, (tenv_body, _, t1)) ->
       (match infer' exp n1 with
        | OK (n2, (tenv_exp, _, exp_type_inferred)) ->
          (match lookup tenv_body var with
           | None -> (* let var not used in body *)
             (match mgu @@ compat [tenv_body; tenv_exp] with
              | UOk s -> OK (n2, (join [tenv_body; tenv_exp], e, t1))
              | UError (t1, t2) -> report t1 t2)
           | Some exp_type_body -> (* let var used *)
             (match mgu @@ (exp_type_body, exp_type_inferred) :: compat [tenv_body; tenv_exp] with
              | UOk s -> OK (n2, (join [tenv_body; tenv_exp;s], e, exp_type_inferred))
              | UError (t1, t2) -> report t1 t2))
        | Error s -> Error s)
     | Error s -> Error s)
  | _ -> failwith @@ "infer': undefined for " ^ string_of_expr e




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
