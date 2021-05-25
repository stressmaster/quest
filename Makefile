MODULES=render main authors texturemap game dungeon state magic_numbers levenshtein fight_menu npc spiral font timer render_stack walker audio item spriteanimation gameover_menu monsters win_menu start_menu
MLIMODULES=render authors texturemap game dungeon state levenshtein fight_menu npc spiral font timer render_stack walker audio item spriteanimation
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MLIMODULES:=.mli)
PNGS= darkness entrance exit goblin_1 monster path player wall timer empty_item armor_pickup weapon_pickup tier_one_armor tier_one_weapon tier_two_armor tier_two_weapon tier_three_armor tier_three_weapon monster4 monster3 ash_path camel_1 npc1 np2 monster5 monster6 monster7 monster8 monster9 monster10 monster11 monster12 camel_2 camel_3 elsa fire_wall flames ice_wall monster3_2 penguin_1 penguin_2 water_path wolf_2 wolf clarkson
IM1=$(PNGS:=.png)
FONTS= fonts/*
IM2=$(FONTS:=.png)
SOUND= camlished unravel oof camlished_battle cruel clarksonvoice
WAV=$(SOUND:=.wav)
TEST=test.byte
MAIN=main.byte
OURMAIN=_build/default/main.bc
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=ounit2,str,yojson,lablgl,camlimages.core,camlimages.png,sdl,sdl.sdlmixer

default: build
	OCAMLRUNPARAM=b utop

build:
	$(OCAMLBUILD) $(OBJECTS)

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