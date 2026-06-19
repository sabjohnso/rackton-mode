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

;;; Type annotations: the LSP type provider

(ert-deftest rackton-lsp-type-provider-quiet-without-eglot ()
  "Unmanaged by eglot, the provider answers nil so the REPL can step in."
  (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil)))
    (should-not (rackton-lsp--type-provider "sqr"))))

(ert-deftest rackton-lsp-type-provider-parses-hover ()
  "The provider reads the `name :: type' hover and returns the bare type."
  (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
            ((symbol-function 'eglot-current-server) (lambda () 'server))
            ((symbol-function 'eglot--TextDocumentPositionParams) (lambda () nil))
            ((symbol-function 'jsonrpc-request)
             (lambda (&rest _)
               '(:contents (:kind "plaintext" :value "sqr :: (-> Integer Integer)")))))
    (should (equal (rackton-lsp--type-provider "sqr") "(-> Integer Integer)"))))

(ert-deftest rackton-lsp-type-provider-nil-on-non-value-hover ()
  "A protocol/type-constructor hover (no `::') yields nil."
  (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
            ((symbol-function 'eglot-current-server) (lambda () 'server))
            ((symbol-function 'eglot--TextDocumentPositionParams) (lambda () nil))
            ((symbol-function 'jsonrpc-request)
             (lambda (&rest _)
               '(:contents (:kind "plaintext" :value "Functor — protocol")))))
    (should-not (rackton-lsp--type-provider "Functor"))))

(ert-deftest rackton-lsp-type-provider-nil-on-null-hover ()
  "No hover at all (a null result) yields nil, not an error."
  (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t))
            ((symbol-function 'eglot-current-server) (lambda () 'server))
            ((symbol-function 'eglot--TextDocumentPositionParams) (lambda () nil))
            ((symbol-function 'jsonrpc-request) (lambda (&rest _) :null)))
    (should-not (rackton-lsp--type-provider "sqr"))))

(ert-deftest rackton-lsp-registers-type-provider ()
  "Loading rackton-lsp adds its provider to `rackton-type-functions',
ahead of any fallback so the LSP is preferred."
  (should (memq 'rackton-lsp--type-provider rackton-type-functions))
  (should (eq 'rackton-lsp--type-provider (car rackton-type-functions))))

(provide 'rackton-lsp-test)
;;; rackton-lsp-test.el ends here
