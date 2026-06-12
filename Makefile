EMACS ?= emacs

.PHONY: test
test:
	$(EMACS) -Q --batch -L . -l tests/rackton-mode-test.el \
	  -l tests/rackton-repl-test.el \
	  -f ert-run-tests-batch-and-exit
