;;; rackton-lsp-test.el --- Tests for rackton-lsp  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the eglot bridge.  eglot is built in, so these run
;; in the batch suite without a network or a Racket install.

;;; Code:

(require 'ert)
(require 'eglot)
(require 'rackton-lsp)

(ert-deftest rackton-lsp-registers-eglot-server ()
  "Loading rackton-lsp points eglot at Rackton's LSP server."
  (let ((entry (assq 'rackton-mode eglot-server-programs)))
    (should entry)
    (should (equal (cdr entry) '("racket" "-l" "rackton/lsp")))))

(ert-deftest rackton-lsp-register-is-idempotent ()
  "Re-registering replaces, never duplicates, the rackton-mode entry."
  (rackton-lsp--register)
  (rackton-lsp--register)
  (should (= 1 (cl-count 'rackton-mode eglot-server-programs :key #'car-safe))))

(provide 'rackton-lsp-test)
;;; rackton-lsp-test.el ends here
