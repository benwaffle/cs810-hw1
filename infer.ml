open Unification
open Subs
open Ast
open Printf


type 'a error = OK of 'a | Error of string

type typing_judgement = subst*expr*texpr

let string_of_typing_judgement tj =
  let (tenv, expr, texpr) = tj in
  Printf.sprintf "\027[31m%s\027[34m ⊢ \027[32m%s \027[34m: \027[33m%s \027[0m"
    (string_of_subs tenv) (string_of_expr expr)(string_of_texpr texpr)

let compat (xs : subst list) : (texpr * texpr) list =
  (* printf "\n\tcompat({";
     List.iter (fun a -> printf "%s, " (string_of_subs a)) xs;
     printf "})\n"; *)
  List.flatten @@ List.flatten @@ List.map (fun s1 ->
      List.map (fun s2 ->
          List.fold_left (fun acc var ->
              match lookup s2 var with
              | Some x -> (Hashtbl.find s1 var, x) :: acc
              | None -> acc
            ) [] (domain s1)
        ) xs
    ) xs

let apply_to_env2 s es =
  List.map (fun e ->
      apply_to_env s e;
      e
    ) es

let apply_to_tj (s : subst) (tj : typing_judgement) : typing_judgement =
  let (tenv, exp, texp) = tj in
  apply_to_env s tenv;
  (tenv, apply_to_expr s exp, apply_to_texpr s texp)

let report t1 t2 =
  Error (Printf.sprintf "cannot unify %s and %s" (string_of_texpr t1) (string_of_texpr t2))

let rec infer' (e:expr) (n:int): (int*typing_judgement) error =
  let op ctor e1 e2 =
    (match infer' e1 n with
     | OK (n1, (s1, e1, t1)) ->
       (match infer' e2 n1 with
        | OK (n2, (s2, e2, t2)) ->
          (match mgu @@ (t1, IntType) :: (t2, IntType) :: (compat [s1;s2]) with
           | UOk s -> OK (n2, apply_to_tj s (join @@ apply_to_env2 s [s1;s2], ctor e1 e2, IntType))
           | UError (t1, t2) -> report t1 t2)
        | err -> err)
     | err -> err) in

  match e with

  | Unit -> OK (n, (create (), e, UnitType))

  | Int x -> OK (n, (create (), e, IntType))

  | Var s -> OK (n+1,
                 let tv = VarType("v" ^ string_of_int n) in
                 let tc = create () in
                 extend tc s tv;
                 (tc, e, tv)
                )

  | Add (e1, e2) -> op (fun a b -> Add (a, b)) e1 e2
  | Sub (e1, e2) -> op (fun a b -> Sub (a, b)) e1 e2
  | Mul (e1, e2) -> op (fun a b -> Mul (a, b)) e1 e2
  | Div (e1, e2) -> op (fun a b -> Div (a, b)) e1 e2

  | IsZero e1 ->
    (match infer' e1 n with
     | OK (n1, (s1, e1, t1)) ->
       (match mgu [(t1, IntType)] with
        | UOk s ->
          apply_to_env s s1;
          OK (n1, apply_to_tj s (s1, IsZero e1, BoolType))
        | UError (t1, t2) -> report t1 t2)
     | err -> err)

  | App (f, x) ->
    (match infer' f n with
     | OK (n1, (s1, f, t1)) ->
       (match infer' x n1 with
        | OK (n2, (s2, x, t2)) ->
          let ret = VarType ("v" ^ string_of_int (n2)) in
          (match mgu @@ (t1, FuncType (t2, ret)) :: compat [s1;s2] with
           | UOk s -> OK (n2+2, apply_to_tj s (join @@ apply_to_env2 s [s1;s2], App (f, x), ret))
           | UError (t1, t2) -> report t1 t2)
        | err -> err)
     | err -> err)

  | Proc (arg, argtype, body) ->
    (match infer' body n with
     | OK (n1, (s1, body, t1)) ->
       let arg_t = (match lookup s1 arg with
           | None -> VarType arg
           | Some t -> t) in
       (match mgu [(arg_t, argtype)] with
        | UOk s ->
          remove s1 arg;
          OK (n1, apply_to_tj s (s1, Proc (arg, argtype, body), FuncType (argtype, t1)))
        | UError (t1, t2) -> report t1 t2)
     | err -> err)

  | ProcUntyped (arg, body) ->
    (match infer' body n with
     | OK (n1, (s1, e1, t1)) ->
       let arg_t = (match lookup s1 arg with
           (* TODO: use fresh type variable *)
           | None -> VarType arg (* arg not used in body, make a VarType *)
           | Some t -> t) in (* arg used in body, get inferred type  *)
       let proc_typed =
         apply_to_expr (let ht = create () in extend ht arg arg_t; ht) @@ ProcUntyped(arg, e1) in (* convert ProcUntyped to Proc *)
       remove s1 arg; (* remove argument type from env because it's scoped *)
       OK (n1, (s1, proc_typed, FuncType (arg_t, t1)))
     | err -> err)

  | Let (var, exp, body) ->
    (match infer' exp n with
     | OK (n1, (tenv_exp, exp, exp_type_inferred)) ->
       (match infer' body n1 with
        | OK (n2, (tenv_body, body, t1)) ->
          let tenv_exp' = Hashtbl.copy tenv_exp in
          remove tenv_exp' var;
          let tenv s =
            remove tenv_body var; (* remove scoped var *)
            join @@ apply_to_env2 s [tenv_body; tenv_exp] in
          let pairs = (match lookup tenv_body var with
              | None -> []
              | Some exp_type_body -> [(exp_type_body, exp_type_inferred)]) in
          (match mgu @@ pairs @ compat [tenv_body; tenv_exp'] with
           | UOk s -> OK (n2, apply_to_tj s (tenv s, Let (var, exp, body), t1))
           | UError (t1, t2) -> report t1 t2)
        | err -> err)
     | err -> err)

  | BeginEnd exprs ->
    let acc = List.fold_left (fun acc expr ->
        match acc with
        | OK (tenvs, n_prev, exprs, typ_prev) ->
          (match infer' expr n_prev with
           | OK (n_new, (tenv, expr, typ)) -> OK (tenv :: tenvs, n_new, expr :: exprs, typ)
           | Error s -> Error s)
        | err -> err
      ) (OK ([], n, [], UnitType)) exprs in
    (match acc with
     | OK (tenvs, n_last, exprs, typ) ->
       (match mgu (compat tenvs) with
        | UOk s -> OK (n_last, apply_to_tj s (join @@ apply_to_env2 s tenvs, BeginEnd exprs, typ))
        | UError (t1, t2) -> report t1 t2)
     | Error s -> Error s)

  | NewRef contents ->
    (match infer' contents n with
     | OK (n1, (tenv, contents, contents_type)) ->
       OK (n1, (tenv, NewRef contents, RefType (contents_type)))
     | err -> err)

  | SetRef (ref, value) ->
    (match infer' ref n with
     | OK (n1, (tenv_ref, ref, ref_type)) ->
       (match infer' value n1 with
        | OK (n2, (tenv_val, value, val_type)) ->
          let contents = VarType ("v"^(string_of_int n2)) in
          (match mgu @@ (ref_type, RefType (contents)) :: (val_type, contents) :: (compat [tenv_ref;tenv_val]) with
           | UOk s -> OK (n2+1, apply_to_tj s (join @@ apply_to_env2 s [tenv_ref;tenv_val], SetRef (ref, value), UnitType))
           | UError (a, b) -> report a b)
        | err -> err)
     | err -> err)

  | DeRef (ref) ->
    (match infer' ref n with
     | OK (n1, (tenv, ref, ref_type)) ->
       let contents = VarType ("v"^(string_of_int n1)) in
       (match mgu [(ref_type, RefType (contents))] with
        | UOk s -> OK (n1+1, apply_to_tj s (tenv, DeRef (ref), contents))
        | UError (a, b) -> report a b)
     | err -> err)

  | LetrecUntyped (func, arg, func_body, in_body) ->
    (match infer' func_body n with
     | OK (n1, (tenv_func_body, func_body, ret_t)) ->
       (match infer' in_body n1 with
        | OK (n2, (tenv_in_body, in_body, body_t)) ->
          let arg_t = (match lookup tenv_func_body arg with
              | None -> VarType ("v"^(string_of_int n2))
              | Some t -> t) in
          let pairs =
            (match lookup tenv_func_body func with
             | None -> [] (* not called recursively *)
             | Some func_type -> [func_type, FuncType (arg_t, ret_t)])
            @
            (match lookup tenv_in_body func with
             | None -> []
             | Some func_type -> [func_type, FuncType (arg_t, ret_t)])
            @
            compat [tenv_func_body; tenv_in_body]
          in
          let letrec_typed = apply_to_expr
              (let ht = create () in
               extend ht func @@ FuncType (arg_t, ret_t);
               extend ht arg arg_t;
               ht) @@ LetrecUntyped (func, arg, func_body, in_body) in
          (match mgu pairs with
           | UOk s ->
             remove tenv_func_body arg;
             remove tenv_func_body func;
             remove tenv_in_body func;
             OK (n2+1, apply_to_tj s (join @@ apply_to_env2 s [tenv_func_body; tenv_in_body], letrec_typed, body_t))
           | UError (a, b) -> report a b)
        | err -> err)
     | err -> err)

  | Letrec (ret_t, func, arg, arg_t, func_body, in_body) ->
    (match infer' func_body n with
     | OK (n1, (tenv_func_body, func_body, ret_t_inferred)) ->
       (match infer' in_body n1 with
        | OK (n2, (tenv_in_body, in_body, body_t)) ->
          let pairs = List.concat [
              [(ret_t, ret_t_inferred)]
              ;
              (match lookup tenv_func_body arg with
               | None -> []
               | Some arg_t_inferred -> [(arg_t, arg_t_inferred)])
              ;
              (match lookup tenv_func_body func with
               | None -> []
               | Some func_type -> [(func_type, FuncType (arg_t, ret_t))])
              ;
              (match lookup tenv_in_body func with
               | None -> []
               | Some func_type -> [(func_type, FuncType (arg_t, ret_t))])
            ] in (match mgu pairs with
              | UOk s ->
                remove tenv_func_body arg;
                remove tenv_func_body func;
                remove tenv_in_body func;
                OK (n2,
                    apply_to_tj s (join @@ apply_to_env2 s [tenv_func_body; tenv_in_body],
                                   Letrec (ret_t, func, arg, arg_t, func_body, in_body),
                                   body_t))
              | UError (a, b) -> report a b)
        | err -> err)
     | err -> err)

  | ITE (cond, then_body, else_body) ->
    (match infer' cond n with
     | OK (n1, (cond_tenv, cond, cond_t)) ->
       (match infer' then_body n1 with
        | OK (n2, (then_tenv, then_body, then_t)) ->
          (match infer' else_body n2 with
           | OK (n3, (else_tenv, else_body, else_t)) ->
             (match mgu @@ (cond_t, BoolType) :: (then_t, else_t) :: compat [cond_tenv; then_tenv; else_tenv] with
              | UOk s -> OK (n3,
                             apply_to_tj s (join @@ apply_to_env2 s [cond_tenv; then_tenv; else_tenv],
                                            ITE (cond, then_body, else_body),
                                            then_t))
              | UError (a, b) -> report a b)
           | err -> err)
        | err -> err)
     | err -> err)


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
