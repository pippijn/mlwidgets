SOURCES := setup.data $(shell find widgets -type f)
TARGETS :=			\
	widgets/widgets.cma	\
	widgets/widgets.cmxa	\
	widgets/widgets.a	\
	testsuite/test.native	\
	tuic/tuic.native

default: all

build:
	ocamlbuild $(TARGETS)

all: $(SOURCES)
	@ocaml setup.ml -build
#	@ocaml setup.ml -doc
#	@ocaml setup.ml -test
#	@ocaml setup.ml -reinstall

setup.data: _oasis
	@oasis setup
	@ocaml setup.ml -configure


%.native: $(SOURCES)
	ocamlbuild $@

%.byte: $(SOURCES)
	ocamlbuild $@

%.js: $(SOURCES)
	ocamlbuild $@

%.inferred.mli: %.ml
	ocamlbuild $@
#	cp _build/$@ $@
