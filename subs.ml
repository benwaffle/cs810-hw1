open Ast

type subst = (string, Ast.texpr) Hashtbl.t

let create () = Hashtbl.create 10

let extend = Hashtbl.add

let remove = Hashtbl.remove

let lookup = Hashtbl.find_opt

let apply_to_texpr set = function
    | x -> x

let apply_to_expr set = function
    | x -> x

let apply_to_env set env = ()

let string_of_subs set =
    Hashtbl.fold (fun k v acc ->
        k ^ " -> " ^ Ast.string_of_texpr(v) ^ ", " ^ acc
    ) set ""

let domain set =
    Hashtbl.fold (fun k v acc -> k :: acc) set []

let rec join sets : subst =
    let combine a b : subst = begin
        Hashtbl.fold (fun k v acc -> Hashtbl.add a k v) b ();
        a
    end in
    match sets with
    | [] -> create ()
    | [x] -> x
    | x :: xs -> combine x (join xs)