TARGET=main
OPTS=-pp camlp4o
TAGS=annot,debug
EXTENSION=byte

all:
	ocamlbuild ${OPTS} -tags ${TAGS} ${TARGET}.${EXTENSION}

clean:
	ocamlbuild -clean
