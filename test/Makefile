export OCAMLMAKEFILE = ../OCamlMakefile

PACKS = ounit
LIBS = ounit str unix ../memcached
INCDIRS = $(shell ocamlfind query ounit) ..
LIBDIRS = ..
SOURCES = memcached_test.ml
RESULT = memcached_test

all: dc test

test:
	./memcached_test

.PHONY: test

-include $(OCAMLMAKEFILE)
