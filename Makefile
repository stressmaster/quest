MODULES=render main authors texturemap game dungeon state magic_numbers levenshtein fight_menu spiral font timer render_stack
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
PNGS= darkness entrance exit goblin_1 monster path player wall
IM1=$(PNGS:=.png)
FONTS= fonts/*
IM2=$(FONTS:=.png)
TEST=test.byte
MAIN=main.byte
OURMAIN=_build/default/main.bc
OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg yojson

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS) && cp $(IM1) ./_build/default && mkdir -p ./_build/default/fonts && cp $(IM2) ./_build/default/fonts

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	ocamlrun ./$(OURMAIN)

zip:
	zip camelquest.zip *.ml* *.json *.png _tags .txt .merlin .ocamlformat .ocamlinit Makefile	dune dune-project .md fonts/*