CAMLC=ocamlc
CAMLOPT=ocamlopt

COMPFLAGS=-ccopt '-O2 -msse3 -msse4.1 -msse4.2'
SRC=$(wildcard *.ml)
TARGETS=$(SRC:.ml=.exe)

.SUFFIXES: .exe .ml .mli .cmi .cmx

all: $(TARGETS)

.c.o:
	$(CAMLC) $(COMPFLAGS) $<

%.exe: %.ml %_stubs.o
#	$(CAMLC) -custom $(COMPFLAGS) unix.cma -o ${@:.exe=_bytecode.exe} \
#	  ${<:.ml=_stubs.o} $<
	$(CAMLOPT) $(COMPFLAGS) -inline 20 -S unix.cmxa -o $@ ${<:.ml=_stubs.o} $<

clean::
	rm -f *.cm* *.exe *.o *.s

.PHONY: all clean
