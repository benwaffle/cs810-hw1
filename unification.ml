open Ast
open Subs
open Printf

type unif_result = UOk of Subs.subst | UError of texpr*texpr

let mgu xs =
  (* printf "unify {";
  List.iter (fun k -> printf "%s =.= %s, " (string_of_texpr @@ fst k) (string_of_texpr @@ snd k)) xs;
  printf "}\n"; *)
  let rec helper (sub:subst) (pairs: (texpr*texpr) list): unif_result =
    match pairs with
    | [] -> UOk (sub)
    | (t1, t2) :: xs ->
      (* printf "\t FV(%s) = {" (string_of_texpr t1); *)
      (* SetStr.iter (printf "%s, ") @@ fv_of_type t1; *)
      (* printf "}\n\t FV(%s) = {" (string_of_texpr t2); *)
      (* printf "\t%s in? FV(%s) = {" (string_of_texpr t1) (string_of_texpr t2); *)
      (* SetStr.iter (printf "%s, ") @@ fv_of_type t2; *)
      (* printf "}\n"; *)
      (* Printf.printf "%s =.= %s\n" (string_of_texpr t1) (string_of_texpr t2); *)
      match (t1, t2) with
      | (FuncType (s1, s2), FuncType (t1, t2)) -> (* decomposition *)
        helper sub @@ (s1, t1) :: (s2, t2) :: xs
      | (x, y) when x = y -> (* trivial pair elimination *)
        helper sub xs
      | (VarType (x), y) when not @@ SetStr.mem x (fv_of_type y) -> (* variable elimination *)
        extend sub x y;
        helper sub (List.map (fun pair ->
          (apply_to_texpr sub @@ fst pair, apply_to_texpr sub @@ snd pair)
        ) xs)
      | (VarType (x), y) -> (* occur check *)
        UError (t1, t2)
      | (x, VarType (y)) -> (* swap *)
        helper sub @@ (t2, t1) :: xs
      | (x, y) -> (* fail, e.g. N = B *)
        UError (x, y)
  in helper (create ()) xs
