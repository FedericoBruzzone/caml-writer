# ocamlfind ocamlopt -c -package camlimages.core <module1>.ml <module2>.ml
# ocamlfind ocamlopt -o program -linkpkg -package camlimages.core <module1>.cmx <module2>.cmx

# ocamlfind ocamlopt -o program -linkpkg -package pkg module1.ml module2.ml

DUNE = dune

CAML_WRITER = caml_writer

all: build

build:
	$(DUNE) build --verbose @all

# --profile release
run:
	$(DUNE) exec $(CAML_WRITER) $(if $(ARGS),$(ARGS),)

clean:
	$(DUNE) clean

.PHONY: all clean

