SHELL := /usr/bin/env bash

EMACS ?= emacs
CASK ?= cask

TEST-FILES := test/bootstrap.el $(shell ls test/parse-it-*.el)
LOAD-FILE = -l $(test-file)
LOAD-TEST-FILES := $(foreach test-file, $(TEST-FILES), $(LOAD-FILE))

build:
	EMACS=$(EMACS) cask install
	EMACS=$(EMACS) cask build
	EMACS=$(EMACS) cask clean-elc

ci: CASK=
ci: compile clean

compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-l test/bootstrap.el \
		-L . -L langs \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile *.el

clean:
	rm -rf .cask *.elc

.PHONY: build ci compile clean
