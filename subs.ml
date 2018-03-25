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

let apply_to_env set env =
  Hashtbl.iter (fun k v ->
      (* evaluate VarTypes  *)
      Hashtbl.replace set k (apply_to_texpr env v)
    ) set;
  Hashtbl.iter (fun k v ->
      (* add from env to set *)
      Printf.printf "adding to %s: %s/%s\n" (string_of_subs set) (string_of_texpr v) k;
      match lookup set k with
      | Some x ->
        if x != v
        then failwith (Printf.sprintf "can't set %s = %s, it's already %s" k (string_of_texpr v) (string_of_texpr x))
      | None -> Hashtbl.add set k v
    ) env

let extend set var texpr =
  let ht = create() in
  Hashtbl.add ht var texpr; (* create tmp hashtbl so we can apply new var *)
  apply_to_env set ht

let remove = Hashtbl.remove

let apply_to_expr set = function
  | x -> x

let domain set =
  Hashtbl.fold (fun k v acc -> k :: acc) set []

(* compose *)
let rec join = function
  | [] -> create ()
  | [x] -> x
  | x :: xs ->
    let res = Hashtbl.copy x in
    apply_to_env res (join xs);
    res