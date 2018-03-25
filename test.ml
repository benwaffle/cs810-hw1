open Printf
open Subs
open Ast

let unify xs =
  match Unification.mgu xs with
  | UError (a, b) -> printf "Error: could not unify %s, %s\n" (string_of_texpr a) (string_of_texpr b)
  | UOk s -> printf "%s\n" @@ string_of_subs s

let () =
  for i = 1 to 24 do
    printf "Test: %s\nType: %s\n========================\n" (Examples.expr i) (Infer.test i)
  done
  ;;

  (* let s = create () in
  extend s "t" @@ FuncType(VarType("u"), BoolType);
  extend s "s" IntType;

  let t = create () in
  extend t "u" @@ FuncType(VarType("x"), IntType);
  extend t "s" IntType;

  printf "joining...\n";
  printf "%s ◦ %s = %s\n" (string_of_subs s) (string_of_subs t) (string_of_subs (join [s;t])) *)

  (* print_string "adding ayy = g -> bool\n";
  Subs.extend x "ayy" (Ast.FuncType ((Ast.VarType "g"), Ast.BoolType));
  printf "%s\n" (Subs.string_of_subs x);
  print_string "adding g = int\n";
  Subs.extend x "g" Ast.IntType;
  printf "%s\n" (Subs.string_of_subs x);
  printf "%s\n" (Ast.string_of_texpr (Subs.apply_to_texpr x @@ VarType("ayy"))) *)

  unify [
    (FuncType (FuncType (IntType, VarType "x"), FuncType (VarType "x", VarType "u"))),
    FuncType (VarType "z", FuncType (FuncType (VarType "y", VarType "y"), VarType "z"))
  ];

  printf "=============\n";

  unify [
    FuncType (VarType "x", FuncType (VarType "y", VarType "x")),
    FuncType (VarType "y", FuncType (FuncType (VarType "x", IntType), VarType "x"))
  ];

  printf "=============\n";

  unify [
    IntType,
    BoolType
  ]