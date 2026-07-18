EMACS ?= emacs

.PHONY: test
# The umbrella suite runs in its own process: it asserts that requiring
# `rackton' alone loads everything, which is only falsifiable when
# nothing else has loaded the individual features first.
test:
	$(EMACS) -Q --batch -L . -l tests/rackton-test.el \
	  -f ert-run-tests-batch-and-exit
	$(EMACS) -Q --batch -L . -l tests/rackton-mode-test.el \
	  -l tests/rackton-repl-test.el \
	  -l tests/rackton-lsp-test.el \
	  -l tests/rackton-dap-test.el \
	  -l tests/rackton-search-test.el \
	  -f ert-run-tests-batch-and-exit
