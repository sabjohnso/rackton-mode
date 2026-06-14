EMACS ?= emacs

.PHONY: test
test:
	$(EMACS) -Q --batch -L . -l tests/rackton-mode-test.el \
	  -l tests/rackton-repl-test.el \
	  -l tests/rackton-lsp-test.el \
	  -l tests/rackton-dap-test.el \
	  -l tests/rackton-search-test.el \
	  -f ert-run-tests-batch-and-exit
