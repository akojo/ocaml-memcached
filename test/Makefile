export OCAMLMAKEFILE = ../OCamlMakefile

PACKS = oUnit
LIBS = oUnit str ../memcached
INCDIRS = $(shell ocamlfind query oUnit) ..
LIBDIRS = ..
SOURCES = memcached_hash_test.ml memcached_test.ml
RESULT = memcached_test

all: nc test

test:
	./memcached_test

.PHONY: test

-include $(OCAMLMAKEFILE)
