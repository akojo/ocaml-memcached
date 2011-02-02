export OCAMLMAKEFILE = ../OCamlMakefile

PACKS = oUnit
LIBS = oUnit str unix ../memcached
INCDIRS = $(shell ocamlfind query oUnit) ..
LIBDIRS = ..
SOURCES = memcached_test.ml
RESULT = memcached_test

all: dc test

test:
	./memcached_test

.PHONY: test

-include $(OCAMLMAKEFILE)
