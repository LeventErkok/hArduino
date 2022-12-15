# (c) Copyright Levent Erkok. All rights reserved.
#
# The hArduino library is distributed with the BSD3 license. See the LICENSE file
# in the distribution for details.
SHELL     := /usr/bin/env bash
TSTSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' | grep -v Setup.hs)
DEPSRCS   = $(shell find . -name '*.hs' -or -name '*.lhs' -or -name '*.cabal' | grep -v Paths_hArduino.hs)
CABAL     = cabal
TIME      = /usr/bin/time

define mkTags
	@find . -name \*.\*hs | xargs fast-tags
endef

.PHONY: all install sdist clean docs hlint tags

all: install

install: $(DEPSRCS) Makefile
	$(call mkTags)
	@$(CABAL) new-install --lib

sdist: install
	$(CABAL) sdist

veryclean: clean

clean:
	@rm -rf dist-newstyle

docs:
	cabal new-haddock --haddock-option=--hyperlinked-source --haddock-option=--no-warnings

release: clean install sdist hlint docs
	@echo "*** hArduino is ready for release!"

hlint: install
	@echo "Running HLint.."
	@hlint System -rhlintReport.html -i "Use otherwise" -i "Parse error"

ghcid:
	ghcid --command="cabal new-repl --repl-options=-Wno-unused-packages"

ci:
	haskell-ci hArduino.cabal --no-tests --no-benchmarks --no-doctest --no-hlint --email-notifications

tags:
	$(call mkTags)
