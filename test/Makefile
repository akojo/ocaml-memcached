export OCAMLMAKEFILE = ../OCamlMakefile

PACKS = oUnit
LIBS = oUnit str
INCDIRS = $(shell ocamlfind query oUnit) ..
SOURCES = ../memcached_hash.mli ../memcached_hash.ml \
	  ../memcached.mli ../memcached.ml \
	  memcached_hash_test.mli memcached_hash_test.ml \
	  memcached_test.ml
RESULT = memcached_test

all: nc test

test:
	./memcached_test

.PHONY: test

clean::
	rm -f *.d *.di

-include $(OCAMLMAKEFILE)
