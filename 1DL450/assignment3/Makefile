all: build

ci: depclean test doc

ZIPFILE = afp_assignment3.zip

DISTFILES = README.txt road.hs dice.hs my_lazy.hs showme.hs indexing.hs

PKG_DESCRIPTION = assignment3.cabal
DEP = .cabal-sandbox
CONFIGURE = dist/setup-config

ifndef PREFIX
	PREFIX = /usr/local
endif

.PHONY: ci install dep configure build test run doc clean depclean

install:
	cabal install \
	  --enable-documentation \
	  --enable-tests \
	  --global \
	  --ignore-sandbox \
	  --haddock-hyperlink-source \
	  --prefix=$(PREFIX) \
	  --reorder-goals \
	  --run-tests

$(DEP): $(PKG_DESCRIPTION)
	[ -e $(DEP) ] || ( \
	  cabal sandbox init && \
	  cabal install threadscope )
	cabal install \
	  --dependencies-only \
	  --enable-documentation \
	  --enable-tests \
	  --haddock-hyperlink-source \
	  --reorder-goals

dep: $(DEP)

$(CONFIGURE): $(DEP)
	cabal configure --enable-tests

configure: $(CONFIGURE)

build: $(CONFIGURE)
	cabal build

test: $(CONFIGURE)
	cabal test --show-details=streaming

run: $(CONFIGURE)
	cabal run

doc: $(CONFIGURE)
	cabal haddock --hyperlink-source

dist:
	zip $(ZIPFILE) $(DISTFILES)

clean:
	rm -rf dist
	rm -rf $(ZIPFILE)

depclean: clean
	rm -rf .cabal-sandbox cabal.sandbox.config

