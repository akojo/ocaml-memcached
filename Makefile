export OCAMLMAKEFILE = OCamlMakefile
export OCAMLDOCFLAGS = -colorize-code -short-functors

ANNOTATE = 1
LIBS = unix str
PACKS = lwt lwt.unix
SOURCES = memcached_hash.mli memcached_hash.ml \
	  memcached.mli memcached.ml
RESULT = memcached

all: bcl ncl

install: libinstall

uninstall: libuninstall

check:
	cd test && make

clean-test:
	cd test && make clean

clean:: clean-test

top: all

ifneq ($(findstring check,$(MAKECMDGOALS)),check)
-include $(OCAMLMAKEFILE)
endif
