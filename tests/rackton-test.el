;;; rackton-test.el --- Tests for the rackton umbrella  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for `rackton', the single entry point that loads the
;; major mode together with every integration.  The contract under test
;; is what a user's init file relies on: one `require' yields every
;; feature, every entry-point command, and a working major mode.
;;
;; These tests are only meaningful in a process where nothing else has
;; loaded the individual features — otherwise they pass no matter what
;; `rackton.el' contains.  The Makefile therefore runs this file in its
;; own `emacs -Q' invocation, before the per-feature suites.

;;; Code:

(require 'ert)
(require 'rackton)

(defun rackton-test--package-features ()
  "Every `rackton-NAME' feature file shipped beside `rackton.el'.
Derived from the directory rather than restated by hand, so a new
integration that `rackton.el' forgets to require fails these tests."
  (let ((directory (file-name-directory (locate-library "rackton"))))
    (delete-dups
     (mapcar (lambda (file)
               (intern (file-name-base file)))
             (directory-files directory nil "\\`rackton-[^/]*\\.elc?\\'")))))

(defconst rackton-test--commands
  '(rackton-mode
    rackton-repl
    rackton-search
    rackton-search-returns
    rackton-search-accepts
    rackton-search-name)
  "Entry-point commands a user expects after loading `rackton'.")

(ert-deftest rackton-loads-every-shipped-feature ()
  "Requiring `rackton' loads every `rackton-NAME' feature in the package."
  (let ((shipped (rackton-test--package-features)))
    (should shipped)                    ; guard against an empty sweep
    (dolist (feature shipped)
      (should (featurep feature)))))

(ert-deftest rackton-provides-entry-point-commands ()
  "Requiring `rackton' makes every user-facing entry point callable."
  (dolist (command rackton-test--commands)
    (should (commandp command))))

(ert-deftest rackton-selects-major-mode-for-lang-line ()
  "Requiring `rackton' alone is enough for a \"#lang rackton\" file to open in the mode."
  (with-temp-buffer
    (insert "#lang rackton\n(define x 1)\n")
    (goto-char (point-min))
    (set-auto-mode)
    (should (eq major-mode 'rackton-mode))))

(ert-deftest rackton-installs-repl-keys-in-mode-map ()
  "Requiring `rackton' binds the REPL and search commands in the mode map."
  (should (eq 'rackton-repl (lookup-key rackton-mode-map (kbd "C-c C-z"))))
  (should (eq 'rackton-eval-defun (lookup-key rackton-mode-map (kbd "C-c C-c"))))
  (should (eq 'rackton-type (lookup-key rackton-mode-map (kbd "C-c C-t")))))

(provide 'rackton-test)
;;; rackton-test.el ends here
