MODULES=render main authors texturemap game dungeon state magic_numbers levenshtein fight_menu npc spiral font timer render_stack walker audio item spriteanimation
MLIMODULES=render authors texturemap game dungeon state levenshtein fight_menu npc spiral font timer render_stack walker audio item spriteanimation
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MLIMODULES:=.mli)
PNGS= darkness entrance exit goblin_1 monster path player wall timer empty_item armor_pickup weapon_pickup tier_one_armor tier_one_weapon 
IM1=$(PNGS:=.png)
FONTS= fonts/*
IM2=$(FONTS:=.png)
SOUND= camlished unravel oof
WAV=$(SOUND:=.wav)
TEST=test.byte
MAIN=main.byte
OURMAIN=_build/default/main.bc
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=ounit2,str,yojson,lablgl,camlimages.core,camlimages.png,sdl,sdl.sdlmixer

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS) && cp $(IM1) ./_build/default && mkdir -p ./_build/default/fonts && cp $(IM2) ./_build/default/fonts && cp $(WAV) ./_build/default

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST) -runner sequential

play:
	ocamlrun ./$(OURMAIN)

zip:
	zip camelquest.zip *.ml* *.json *.png _tags *.txt .merlin *.wav .ocamlformat .ocamlinit Makefile	dune dune-project *.md fonts/*

docs: docs-public docs-private

docs-public: build
	mkdir -p _doc.public
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.public $(MLIS)

docs-private: build
	mkdir -p _doc.private
	ocamlfind ocamldoc -I _build -package $(PKGS) \
		-html -stars -d _doc.private \
		-inv-merge-ml-mli -m A -hide-warnings $(MLIS) $(MLS)