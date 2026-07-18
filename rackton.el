;;; rackton.el --- Rackton development environment  -*- lexical-binding: t; -*-

;; Author: Samuel B. Johnson <samuel.bryant.johnson@gmail.com>
;; Version: 0.6.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages, lisp, tools

;;; Commentary:

;; The single entry point to Rackton's Emacs support.  One line in an
;; init file:
;;
;;   (require 'rackton)
;;
;; loads the major mode and every integration built on it:
;;
;;   `rackton-mode'   the major mode: font-lock, indentation, imenu
;;   `rackton-repl'   the inferior REPL and its evaluation commands
;;   `rackton-lsp'    the eglot bridge to Rackton's Language Server
;;   `rackton-dap'    the dape bridge to Rackton's debug server
;;   `rackton-search' Hoogle-style search over the standard library
;;
;; Loading them all costs little: the eglot and dape hookups install
;; themselves under `with-eval-after-load', so neither package is
;; pulled in, and nothing external is started, until it is actually
;; present and used.  Measured cost of the whole umbrella,
;; byte-compiled, is roughly 20 ms, of which about 11 ms is `comint'
;; loading for the REPL.
;;
;; This file contains no forms of its own beyond the requires, but it
;; is not without effect: loading the set populates
;; `rackton-type-functions' (the LSP and REPL type providers),
;; `rackton-mode-hook' (the REPL's eldoc and completion setup),
;; `magic-mode-alist' (the "#lang rackton" entry), and
;; `rackton-mode-map' (the REPL and search keybindings).  Requiring
;; `rackton-mode' by itself yields only the major mode, so anyone
;; wanting a narrower set should keep requiring the features
;; individually.
;;
;; Note that this umbrella requires Emacs 28.1, the floor set by the
;; REPL, LSP, and debug bridges; `rackton-mode' alone still runs on
;; 27.1.

;;; Code:

(require 'rackton-mode)
(require 'rackton-repl)
(require 'rackton-lsp)
(require 'rackton-dap)
(require 'rackton-search)

(provide 'rackton)
;;; rackton.el ends here
