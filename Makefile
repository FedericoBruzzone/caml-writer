# ocamlfind ocamlopt -c -package camlimages.core <module1>.ml <module2>.ml
# ocamlfind ocamlopt -o program -linkpkg -package camlimages.core <module1>.cmx <module2>.cmx


# ocamlfind ocamlopt -o program -linkpkg -package pkg module1.ml module2.ml

# Makefile for compiling expressions.ml and new.ml separately

DUNE = dune

CAML_WRITER = caml_writer 

all: build

build: 	
	$(DUNE) build @all

run: 
	$(DUNE) exec $(CAML_WRITER)

clean:
	$(DUNE) clean	

.PHONY: all clean
