include Makefile.config

all:
	./compile

.PHONY: install

install: uninstall
	install -m 755 -d $(PREFIX) $(PREFIX)/bin
	cp _build/src/bf.native $(PREFIX)/bin/bf
	cp tools/make-autologin $(PREFIX)/bin/make-autologin

.PHONY: uninstall
uninstall:
	rm -f $(PREFIX)/bin/bf
	rm -r $(PREFIX)/bin/make-autologin

.PHONY: clean
clean:
	rm -rf _build

.PHONY: distclean
distclean: clean
	rm -f *~ Makefile.config
	rm -rf _build
