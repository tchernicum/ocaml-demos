CAMLC=../../ocaml/boot/ocamlrun ../../ocaml/ocamlc
CAMLOPT=../../ocaml/boot/ocamlrun ../../ocaml/ocamlopt

COMPFLAGS=-I ../../ocaml/stdlib -I ../../ocaml/otherlibs/unix -ccopt -O2
SRC=$(wildcard *.ml)
TARGETS=$(SRC:.ml=.exe)

.SUFFIXES: .exe .ml .mli .cmi .cmx

all: $(TARGETS)

.ml.exe:
	$(CAMLC) $(COMPFLAGS) ${<:.ml=_stubs.c}
#	$(CAMLC) -custom $(COMPFLAGS) unix.cma -o ${@:.exe=_bytecode.exe} \
#	  ${<:.ml=_stubs.o} $<
	$(CAMLOPT) $(COMPFLAGS) -inline 20 -S unix.cmxa -o $@ ${<:.ml=_stubs.o} $<

clean::
	rm -f *.cm* *.exe *.o *.s

.PHONY: all clean
