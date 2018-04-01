open Ast

type subst = (string, Ast.texpr) Hashtbl.t

let create () = Hashtbl.create 10

let lookup = Hashtbl.find_opt

let string_of_subs set =
  "{" ^
  (Hashtbl.fold (fun k v acc ->
       Ast.string_of_texpr(v) ^ "/" ^ k ^ ", " ^ acc
     ) set "")
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
    if x != texpr
    then failwith (Printf.sprintf "can't set %s = %s, it's already %s" var (string_of_texpr texpr) (string_of_texpr x))
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
    | Some t -> Proc (v, t, apply_to_expr set b)
    | None -> ProcUntyped (v, apply_to_expr set b))
  | App (f, x) -> App (apply_to_expr set f, apply_to_expr set x)
  | _ -> e

let domain set =
  Hashtbl.fold (fun k v acc -> k :: acc) set []

let join (xs: subst list) : subst =
  List.fold_left (fun acc sub ->
    Hashtbl.iter (fun k v ->
      match lookup sub k with
      | Some x when x != v -> failwith @@ Printf.sprintf "join failed; %s != %s" (string_of_texpr x) (string_of_texpr v)
      | _ -> extend acc k v
    ) sub;
    acc
  ) (create()) xs