# make             build ./esploro, the command (needs SBCL alone)
# make test        the core's tests and the window's (SBCL alone; Emacs in batch)
# make install     ./esploro into ~/.local/bin; emacs/esploro.el is loaded from
#                  where it is (see README)

SBCL ?= sbcl
EMACS ?= emacs
PREFIX ?= $(HOME)/.local

esploro: esploro.asd build.lisp $(wildcard src/*.lisp)
	$(SBCL) --noinform --non-interactive --no-userinit --load build.lisp
	mv -f esploro.new esploro

test: esploro
	$(SBCL) --script tests/run.lisp
	$(EMACS) --batch -Q -L emacs -l tests/esploro-tests.el -f ert-run-tests-batch-and-exit

install: esploro
	install -D -m 755 esploro $(PREFIX)/bin/esploro

.PHONY: test install
