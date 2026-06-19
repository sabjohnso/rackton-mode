;;; rackton-lsp.el --- eglot integration for Rackton  -*- lexical-binding: t; -*-

;; Author: Samuel B. Johnson <samuel.bryant.johnson@gmail.com>
;; Version: 0.4.13
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages, tools

;;; Commentary:

;; Bridges `rackton-mode' buffers to Rackton's Language Server,
;; `racket -l rackton/lsp'.  Once eglot connects, the server provides
;; diagnostics, hover, completion, go-to-definition, and document
;; symbols, which eglot surfaces through Emacs' own flymake, eldoc,
;; completion-at-point, xref, and imenu.
;;
;; The dependency on eglot is optional and lazy: the server program is
;; registered only when eglot itself loads, so requiring this file adds
;; no hard dependency and does nothing until eglot is present.  Connect
;; per buffer with `M-x eglot', or automatically:
;;
;;   (add-hook 'rackton-mode-hook #'eglot-ensure)
;;
;; When eglot manages a buffer its hover becomes the source of
;; type-at-point; the REPL's eldoc steps aside (see `rackton-repl').

;;; Code:

(require 'rackton-mode)

(defvar eglot-server-programs)          ; defined by eglot when it loads
(declare-function eglot-managed-p "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function jsonrpc-request "jsonrpc")

(defun rackton-lsp--register ()
  "Point eglot at Rackton's LSP server for `rackton-mode' buffers.
Idempotent: replaces any existing `rackton-mode' entry rather than
adding a second.  The command is built from `rackton-program', so a
customized Racket binary is honored."
  (when (boundp 'eglot-server-programs)
    (setf (alist-get 'rackton-mode eglot-server-programs)
          (list rackton-program "-l" "rackton/lsp"))))

(with-eval-after-load 'eglot (rackton-lsp--register))

;;; Type source

(defun rackton-lsp--hover-value (response)
  "The plain text of an LSP Hover RESPONSE, or nil.
Handles the MarkupContent the Rackton server sends — a plist with a
`:value' — as well as a bare string; a null result yields nil."
  (let ((contents (and (listp response) (plist-get response :contents))))
    (cond ((stringp contents) contents)
          ((and (listp contents) (stringp (plist-get contents :value)))
           (plist-get contents :value)))))

(defun rackton-lsp--type-provider (_name)
  "Provide the type at point from the LSP, or nil — a `rackton-type-functions'.
Answers only when eglot manages the buffer, reading the `name :: type'
hover the Rackton server returns at point (where the bound name is) and
parsing it with `rackton--scheme-type'.  NAME is unused: the hover is
located by position, not by name."
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (when-let* ((server (eglot-current-server))
                (value (rackton-lsp--hover-value
                        (jsonrpc-request server :textDocument/hover
                                         (eglot--TextDocumentPositionParams)))))
      (rackton--scheme-type value))))

;; At the front, so a live LSP is preferred over the REPL fallback.
(add-hook 'rackton-type-functions #'rackton-lsp--type-provider)

(provide 'rackton-lsp)
;;; rackton-lsp.el ends here
