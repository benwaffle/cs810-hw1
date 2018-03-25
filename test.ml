open Printf

let () =
  (* for i = 1 to 24 do
    printf "====================\nTest: %s\nType: %s\n" (Examples.expr i) (Infer.test i)
  done *)
  let x = Subs.create () in begin
    print_string "adding ayy = g -> bool\n";
    Subs.extend x "ayy" (Ast.FuncType ((Ast.VarType "g"), Ast.BoolType));
    printf "%s\n" (Subs.string_of_subs x);
    print_string "adding g = int\n";
    Subs.extend x "g" Ast.IntType;
    printf "%s\n" (Subs.string_of_subs x);
    printf "%s\n" (Ast.string_of_texpr (Subs.apply_to_texpr x @@ VarType("ayy")))
  end
