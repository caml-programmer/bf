include Makefile.config
SOURCES   = system.ml scheme.ml params.ml logger.ml git.ml rules.ml commands.ml
OBJECTS   = $(SOURCES:.ml=.cmo)
XOBJECTS  = $(OBJECTS:.cmo=.cmx)
ARCHIVE   = archive.cma
XARCHIVE  = $(ARCHIVE:.cma=.cmxa)
REQUIRES  = getopt pcre shell ocs

all: $(ARCHIVE)
	$(OCAMLC) -o bf -custom toplevellib.cma $(ARCHIVE) bf.ml -linkpkg

opt: $(XARCHIVE)
	$(OCAMLOPT) -o bf $(XARCHIVE) bf.ml -linkpkg

$(ARCHIVE): $(OBJECTS)
	$(OCAMLC) -a -o $(ARCHIVE) $(OBJECTS)

$(XARCHIVE): $(XOBJECTS)
	$(OCAMLOPT) -a -o $(XARCHIVE) $(XOBJECTS)

OPTIONS   = -syntax camlp4o -package camlp4
OCAMLC    = $(OCAMLFIND) ocamlc   -w ys $(OPTIONS) -package "$(REQUIRES)"
OCAMLOPT  = $(OCAMLFIND) ocamlopt -w ys $(OPTIONS) -package "$(REQUIRES)"
OCAMLDEP  = $(OCAMLFIND) ocamldep $(OPTIONS)
OCAMLLEX  = ocamllex
OCAMLFIND = ocamlfind

depend: $(SOURCES)
	$(OCAMLDEP) $(SOURCES) $(APPNAME).ml >depend

.PHONY: install
install:
	install -m 755 -d $(PREFIX) $(PREFIX)/bin
	install -m 755 bf $(PREFIX)/bin

.PHONY: uninstall
uninstall:
	rm -f $(PREFIX)/bin/bf

.PHONY: clean
clean:
	rm -f *~ *.cmi *.cmo *.cma *.cmx *.o *.a *.cmxa

.PHONY: distclean
distclean: clean
	rm -f *~ depend Makefile.config
	rm -f bf

.SUFFIXES: .cmo .cmi .cmx .ml .mli .mll

.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.cmo:
	$(OCAMLC) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

.mll.ml:
	$(OCAMLLEX) $<

*.mli:
	true

include depend








