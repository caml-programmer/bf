include Makefile.config

.PHONY: install clean distclean uninstall test

all: build test

build:
	echo 'Make build started'
	./compile
	echo 'Make build finished'

environment:
	opam init -y
	eval `opam config env`
	opam install ocamlfind -y
	opam install yojson -y
	eval `opam config env`
	echo 'Environment created'

install:
	install -m 755 -d $(PREFIX) $(PREFIX)/bin
	install -m 755 _build/src/bf.native $(PREFIX)/bin/bf
	install -m 755 tools/make-autologin $(PREFIX)/bin/make-autologin

test:
	echo 'make test started'
	mkdir -p .tests && cd .tests && ../_build/src/bf.native tests
	echo 'make test finished'

uninstall:
	rm -f $(PREFIX)/bin/bf
	rm -r $(PREFIX)/bin/make-autologin

clean:
	rm -rf _build

distclean: clean
	rm -f *~ Makefile.config
	rm -rf _build

