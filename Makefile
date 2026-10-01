# make           build ./esploro (needs SBCL and Quicklisp)
# make test      the core's tests (SBCL alone: no X, no McCLIM)
# make install   ./esploro into ~/.local/bin

SBCL ?= sbcl
QUICKLISP ?= $(HOME)/quicklisp/setup.lisp
PREFIX ?= $(HOME)/.local

esploro: esploro.asd build.lisp $(wildcard src/*.lisp)
	$(SBCL) --noinform --non-interactive --load $(QUICKLISP) --load build.lisp
	mv -f esploro.new esploro

test:
	$(SBCL) --script tests/run.lisp

install: esploro
	install -D -m 755 esploro $(PREFIX)/bin/esploro

.PHONY: test install
