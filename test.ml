open Printf
open Subs
open Ast

let () =
  for i = 1 to 50 do
    printf "Test: %s\nType: %s\n========================\n" (Examples.expr i) (Infer.test i)
  done
;;