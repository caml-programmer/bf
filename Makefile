include Makefile.config

.PHONY: install clean distclean uninstall test

all: build test

build:
	./compile

install:
	install -m 755 -d $(PREFIX) $(PREFIX)/bin
	install -m 755 _build/src/bf.native $(PREFIX)/bin/bf
	install -m 755 tools/make-autologin $(PREFIX)/bin/make-autologin

test:
	./_build/src/bf.native tests

uninstall:
	rm -f $(PREFIX)/bin/bf
	rm -r $(PREFIX)/bin/make-autologin

clean:
	rm -rf _build

distclean: clean
	rm -f *~ Makefile.config
	rm -rf _build
