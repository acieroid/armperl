TARGET=lexer
OPTS=-pp camlp4o

all:
	ocamlbuild ${OPTS} ${TARGET}.native
