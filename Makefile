# make             build ./esploro, the command (needs SBCL alone)
# make test        the core's tests and the window's (SBCL alone; Emacs in batch)
# make install     ./esploro into ~/.local/bin; emacs/esploro.el is loaded from
#                  where it is (see README)
# make info        doc/esploro.info, the manual, from doc/esploro.texi (committed,
#                  so a build needs no makeinfo)

SBCL ?= sbcl
EMACS ?= emacs
PREFIX ?= $(HOME)/.local

esploro: esploro.asd build.lisp $(wildcard src/*.lisp)
	$(SBCL) --noinform --non-interactive --no-userinit --load build.lisp
	mv -f esploro.new esploro

doc/esploro.info: doc/esploro.texi
	makeinfo --no-split --fill-column=64 -o $@ $<

info: doc/esploro.info

test: esploro
	$(SBCL) --script tests/run.lisp
	@if command -v makeinfo >/dev/null; then \
	  d=$$(mktemp -d) && makeinfo --no-split --fill-column=64 -o $$d/esploro.info doc/esploro.texi && \
	  cmp -s $$d/esploro.info doc/esploro.info; r=$$?; rm -rf $$d; \
	  [ $$r = 0 ] || { echo "FAIL: doc/esploro.info is older than its .texi: make info"; exit 1; }; \
	  echo "the manual: built from its source, as committed"; fi
	$(EMACS) --batch -Q -L emacs -l tests/esploro-tests.el -f ert-run-tests-batch-and-exit

install: esploro
	install -D -m 755 esploro $(PREFIX)/bin/esploro

.PHONY: test install info
