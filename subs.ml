open Ast

type subst = (string, Ast.texpr) Hashtbl.t

let create () = Hashtbl.create 10

let lookup = Hashtbl.find_opt

let rec apply_to_texpr set = function
  | IntType -> IntType
  | BoolType -> BoolType
  | UnitType -> UnitType
  | VarType s -> (
    match lookup set s with
    | Some t -> t
    | None -> begin
      print_string ("none " ^ s ^ "\n");
      VarType s
    end
  )
  | FuncType (arg, ret) -> FuncType (apply_to_texpr set arg, apply_to_texpr set ret)
  | RefType t -> RefType (apply_to_texpr set t)

let apply_to_env set env = begin
  Hashtbl.iter (fun k v ->
    (* evaluate VarType's  *)
    Hashtbl.replace set k (apply_to_texpr env v)
  ) set;
  Hashtbl.iter (fun k v ->
    (* add from env to set *)
    Hashtbl.add set k v
  ) env;
  ()
end

let extend set var texpr =
  let ht = create() in begin
    (* Hashtbl.add set var texpr; (* add to subst *)  *)
    Hashtbl.add ht var texpr; (* create tmp hashtbl so we can apply new var *)
    apply_to_env set ht;
    ()
  end

let remove = Hashtbl.remove

let apply_to_expr set = function
  | x -> x

let string_of_subs set =
  "{" ^
  (Hashtbl.fold (fun k v acc ->
      Ast.string_of_texpr(v) ^ "/" ^ k ^ ", " ^ acc
  ) set "")
  ^ "}"

let domain set =
  Hashtbl.fold (fun k v acc -> k :: acc) set []

(* compose *)
let rec join sets : subst =
  let combine a b : subst = begin
    Hashtbl.fold (fun k v acc -> Hashtbl.add a k v) b ();
    a
  end in
  match sets with
  | [] -> create ()
  | [x] -> x
  | x :: xs -> combine x (join xs)