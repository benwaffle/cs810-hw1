.PHONY: all clean infer test repl utop

OCB_FLAGS = -tag bin_annot -use-menhir -use-ocamlfind -pkgs oUnit
OCB = ocamlbuild $(OCB_FLAGS)

infer:
	$(OCB) infer.byte

clean:
	$(OCB) -clean

utop: infer
	utop

test:
	$(OCB) test.byte && ./test.byte
