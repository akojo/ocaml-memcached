export OCAMLMAKEFILE = OCamlMakefile
export OCAMLDOCFLAGS = -colorize-code -short-functors

LIBS = unix str
SOURCES = memcached_native.c memcached.mli memcached.ml
RESULT = memcached

all: bcl ncl

install: libinstall

uninstall: libuninstall

check: all
	cd test && make

clean-test:
	cd test && make clean

clean:: clean-test

top: all

-include $(OCAMLMAKEFILE)
