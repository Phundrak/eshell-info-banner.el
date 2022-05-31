# -*- indent-tabs-mode: t -*-

export EMACS ?= $(shell which emacs)
CASK ?= $(shell which cask)

all: compile

compile:
	${CASK} exec ${EMACS} -Q --script bin/compile-package.el 2>&1 | grep -A 2 -E "([Ee]rror|[Ww]arning):" && exit 1 || exit 0

.PHONY: all compile
