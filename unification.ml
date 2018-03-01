open Ast


type  unif_result = UOk of Subs.subst | UError of texpr*texpr

