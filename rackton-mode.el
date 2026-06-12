;;; rackton-mode.el --- Major mode for the Rackton language  -*- lexical-binding: t; -*-

;; Author: Samuel B. Johnson <samuel.bryant.johnson@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, lisp

;;; Commentary:

;; A major mode for editing Rackton, a statically-typed functional
;; language (in the style of Coalton) embedded in Racket.  Rackton
;; source is s-expression syntax, so this mode derives from
;; `scheme-mode' and layers on:
;;
;;   - selection of `rackton-mode' for files beginning "#lang rackton";
;;   - font-lock for Rackton's surface forms, type signatures, and the
;;     convention that capitalized names are types or constructors;
;;   - indentation rules for Rackton's special forms, including the
;;     `do' style used throughout the rackton repository.
;;
;; All indentation knowledge lives in a buffer-local table consulted by
;; `rackton--indent-function'; nothing is `put' on symbols shared with
;; scheme-mode, so loading this mode never changes how Scheme buffers
;; indent.

;;; Code:

(require 'scheme)

;;; Surface forms
;;
;; These lists are the single statement of which names Rackton treats
;; specially.  Font-lock and indentation both read from them; the
;; reference list is scribblings/reference/syntax-forms.scrbl in the
;; rackton repository.

(defconst rackton-definition-forms
  '("data" "struct" "newtype" "define" "define-alias" "define-effect"
    "class" "instance" "protocol" "foreign" "foreign-c")
  "Forms that introduce definitions at the top of a Rackton module.")

(defconst rackton-expression-forms
  '("match" "match-let" "do" "let" "let*" "let+" "letrec"
    "lambda" "case-lambda" "cond" "if" "ann" "delay"
    "handle" "escape" "proc" "rec" "feed" "update" "via" "racket")
  "Forms that head Rackton expressions.")

(defconst rackton-module-forms
  '("require" "only-in"
    "provide" "all-defined-out" "all-from-out" "data-out" "struct-out"
    "protocol-out" "rename-out" "except-out")
  "Module import/export forms and their spec sub-form introducers.
See the \"Module forms\" and \"provide-specs\" sections of the Rackton
reference.")

;;; Font-lock

(defconst rackton--type-name-regexp
  "\\_<[A-Z][[:alnum:]!?_/-]*\\_>"
  "Capitalized identifiers: types and data constructors by convention.")

(defconst rackton-font-lock-keywords
  `((,(concat "(" (regexp-opt (append rackton-definition-forms
                                      rackton-expression-forms
                                      rackton-module-forms)
                              'symbols))
     (1 font-lock-keyword-face))
    ;; (: name type) — a top-level type signature.
    ("(\\(:\\)[ \t\n]+\\(\\(?:\\sw\\|\\s_\\)+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    (,rackton--type-name-regexp . font-lock-type-face))
  "Font-lock rules layered on top of those inherited from scheme-mode.")

;;; Indentation
;;
;; scheme-mode indents through the buffer-local variable
;; `lisp-indent-function'.  We install our own function there; it
;; consults `rackton-indent-specs' and falls back to
;; `scheme-indent-function' for everything else.

(defconst rackton-indent-specs
  '((match     . 1)
    (match-let . 1)
    (class     . 1)
    (instance  . 1)
    (protocol  . 1)
    (data      . 1)
    (struct    . 1)
    (newtype   . 1)
    (handle    . 1)
    (update    . 1)
    (proc      . 1)
    (let+      . 1)
    (describe  . 1)                     ; rackton/unit: (describe "name" test ...)
    (cond      . 0)
    (racket    . 2)                     ; (racket type (var ...) body ...)
    (do        . rackton--indent-do))
  "How to indent Rackton special forms.
An integer N means N distinguished arguments followed by a body, as in
`lisp-indent-specform'.  A function is called as (FN STATE INDENT-POINT
NORMAL-INDENT) — the same signature scheme-mode uses for function-valued
`scheme-indent-function' properties — and returns a column.")

(defun rackton--form-head (state)
  "Return the symbol heading the innermost form open at STATE, or nil."
  (let ((containing (nth 1 state)))
    (when containing
      (save-excursion
        (goto-char (1+ containing))
        (when (looking-at "\\(?:\\sw\\|\\s_\\)+")
          (intern-soft (match-string-no-properties 0)))))))

(defun rackton--indent-do (state indent-point normal-indent)
  "Indent a line inside a Rackton `do' form.
Binding clauses (lines opening with \"[\") align under the first
binding, which is NORMAL-INDENT; the trailing body expression sits
at `lisp-body-indent'.  STATE and INDENT-POINT are as for
`lisp-indent-function'."
  (save-excursion
    (goto-char indent-point)
    (skip-chars-forward " \t")
    (if (eq (char-after) ?\[)
        normal-indent
      (goto-char (nth 1 state))
      (+ (current-column) lisp-body-indent))))

(defun rackton--indent-function (indent-point state)
  "Indent according to `rackton-indent-specs', else as Scheme.
INDENT-POINT and STATE are as for `lisp-indent-function'."
  ;; Like `scheme-indent-function', the default alignment is the column
  ;; where `calculate-lisp-indent' left point.
  (let ((normal-indent (current-column))
        (spec (cdr (assq (rackton--form-head state) rackton-indent-specs))))
    (cond ((functionp spec) (funcall spec state indent-point normal-indent))
          ((integerp spec)
           (lisp-indent-specform spec state indent-point normal-indent))
          (t (scheme-indent-function indent-point state)))))

;;; Mode

;;;###autoload
(define-derived-mode rackton-mode scheme-mode "Rackton"
  "Major mode for editing Rackton code.

Rackton is a statically-typed functional language embedded in
Racket.  See the rackton repository's documentation for the
language itself."
  (setq-local lisp-indent-function #'rackton--indent-function)
  (setq-local indent-tabs-mode nil)
  ;; Rackton, like Racket, is case-sensitive; scheme-mode's
  ;; font-lock-defaults set CASE-FOLD to t, which would make the
  ;; capitalized-name rule match every identifier.
  (let ((defaults (copy-sequence font-lock-defaults)))
    (setf (nth 2 defaults) nil)
    (setq-local font-lock-defaults defaults))
  (font-lock-add-keywords nil rackton-font-lock-keywords))

;;;###autoload
(add-to-list 'magic-mode-alist
             '("\\`#lang rackton\\(?:[[:space:]]\\|$\\)" . rackton-mode))

(provide 'rackton-mode)
;;; rackton-mode.el ends here
