open Ast

type subst = (string, Ast.texpr) Hashtbl.t

let create () = Hashtbl.create 10

let lookup = Hashtbl.find_opt

let domain set =
  Hashtbl.fold (fun k v acc -> k :: acc) set []

let string_of_subs set =
  match Hashtbl.length set with
  | 0 -> "{}"
  | _ ->
    let keys = domain set in
    "{"
    ^ List.fold_left
      (fun acc k -> Printf.sprintf "%s, %s/%s" acc (Ast.string_of_texpr @@ Hashtbl.find set k) k)
      (Printf.sprintf "%s/%s" (Ast.string_of_texpr @@ Hashtbl.find set @@ List.hd keys) (List.hd keys))
      (List.tl keys)
    ^ "}"

let rec apply_to_texpr set = function
  | IntType -> IntType
  | BoolType -> BoolType
  | UnitType -> UnitType
  | VarType s -> (
      match lookup set s with
      | Some t -> t
      | None -> VarType s
    )
  | FuncType (arg, ret) -> FuncType (apply_to_texpr set arg, apply_to_texpr set ret)
  | RefType t -> RefType (apply_to_texpr set t)

let extend set var texpr =
  let ht = create() in
  Hashtbl.add ht var texpr; (* create temp hashtbl so we can apply new var *)
  Hashtbl.iter (fun k v ->
      Hashtbl.replace set k (apply_to_texpr ht v)
    ) set;
  match lookup set var with
  | Some x ->
    if x <> texpr
    then failwith (Printf.sprintf "on %s, can't set %s = %s, it's already %s" (string_of_subs set) var (string_of_texpr texpr) (string_of_texpr x))
  | None -> Hashtbl.add set var texpr

let apply_to_env set env =
  Hashtbl.iter (fun k v ->
      Hashtbl.replace env k (apply_to_texpr set v)
    ) env

let remove = Hashtbl.remove

let rec apply_to_expr set e =
  match e with
  | Unit | Var _ | Int _ -> e
  | Add (a, b) -> Add (apply_to_expr set a, apply_to_expr set b)
  | Sub (a, b) -> Sub (apply_to_expr set a, apply_to_expr set b)
  | Mul (a, b) -> Mul (apply_to_expr set a, apply_to_expr set b)
  | Div (a, b) -> Div (apply_to_expr set a, apply_to_expr set b)
  | Let (v, e, b) -> Let (v, apply_to_expr set e, apply_to_expr set b)
  | IsZero e -> IsZero (apply_to_expr set e)
  | ITE (i, t, e) -> ITE (apply_to_expr set i, apply_to_expr set t, apply_to_expr set e)
  | Proc (v, t, b) -> Proc (v, apply_to_texpr set t, apply_to_expr set b)
  | ProcUntyped (v, b) ->
    (match lookup set v with
     | Some t -> apply_to_expr set (Proc (v, t, b))
     | None -> ProcUntyped (v, apply_to_expr set b))
  | App (f, x) -> App (apply_to_expr set f, apply_to_expr set x)
  | Letrec (ret_t, func, arg, arg_t, func_body, in_body) ->
    Letrec (apply_to_texpr set ret_t, func, arg, apply_to_texpr set arg_t, apply_to_expr set func_body, apply_to_expr set in_body)
  | LetrecUntyped (func, var, func_body, in_body) ->
    (match lookup set func with (* lookup type of recursive func *)
     | Some (FuncType (arg, ret)) ->
       (match lookup set var with (* lookup type of arg *)
        | Some arg2 when arg2 = arg ->
          apply_to_expr set @@ Letrec (ret, func, var, arg, func_body, in_body)
        | _ ->
          LetrecUntyped (func, var, apply_to_expr set func_body, apply_to_expr set in_body))
     | _ ->
       LetrecUntyped (func, var, apply_to_expr set func_body, apply_to_expr set in_body))
  | BeginEnd xs -> BeginEnd (List.map (apply_to_expr set) xs)
  | NewRef e -> NewRef (apply_to_expr set e)
  | DeRef e -> DeRef (apply_to_expr set e)
  | SetRef (r, v) -> SetRef (apply_to_expr set r, apply_to_expr set v)

open Printf

let join (xs: subst list) : subst =
  List.fold_left (fun acc sub ->
      Hashtbl.iter (fun k v ->
          match lookup sub k with
          | Some x when x <> v -> failwith @@ Printf.sprintf "join failed; %s != %s" (string_of_texpr x) (string_of_texpr v)
          | _ -> extend acc k v
        ) sub;
      acc
    ) (create()) xs