MODULES=render main authors texturemap game dungeon state magic_numbers
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
#TEST=test.byte#
MAIN=main.byte
OURMAIN=_build/default/main.bc
OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg yojson

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	ocamlrun ./$(OURMAIN)