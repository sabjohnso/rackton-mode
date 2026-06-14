;;; rackton-lsp.el --- eglot integration for Rackton  -*- lexical-binding: t; -*-

;; Author: Samuel B. Johnson <samuel.bryant.johnson@gmail.com>
;; Version: 0.4.12
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

(defun rackton-lsp--register ()
  "Point eglot at Rackton's LSP server for `rackton-mode' buffers.
Idempotent: replaces any existing `rackton-mode' entry rather than
adding a second.  The command is built from `rackton-program', so a
customized Racket binary is honored."
  (when (boundp 'eglot-server-programs)
    (setf (alist-get 'rackton-mode eglot-server-programs)
          (list rackton-program "-l" "rackton/lsp"))))

(with-eval-after-load 'eglot (rackton-lsp--register))

(provide 'rackton-lsp)
;;; rackton-lsp.el ends here
