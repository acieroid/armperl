TARGET=main
OPTS=-pp camlp4o
TAGS=annot,debug

all:
	ocamlbuild ${OPTS} -tags ${TAGS} ${TARGET}.native
