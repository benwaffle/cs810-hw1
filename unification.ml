open Ast

type unif_result = UOk of Subs.subst | UError of texpr*texpr

let mgu xs = UOk (Subs.create ())