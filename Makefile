export EMACS ?= $(shell which emacs)
EASK ?= $(shell which eask)

all: install compile lint

lint: checkdoc indent package regexps declare

install:
	${EASK} install-deps
	${EASK} install

compile:
	${EASK} compile
	${EASK} clean-elc

checkdoc:
	${EASK} lint checkdoc

declare:
	${EASK} lint declare

indent:
	${EASK} lint indent

package:
	${EASK} lint package

regexps:
	${EASK} lint regexps

.PHONY: all install compile ci
