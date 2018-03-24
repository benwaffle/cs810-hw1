open Printf

let () =
  for i = 1 to 24 do
    printf "====================\nTest: %s\nType: %s\n" (Examples.expr i) (Infer.test i)
  done
