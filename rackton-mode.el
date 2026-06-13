;;; rackton-mode.el --- Major mode for the Rackton language  -*- lexical-binding: t; -*-

;; Author: Samuel B. Johnson <samuel.bryant.johnson@gmail.com>
;; Version: 0.4.6
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

(defgroup rackton nil
  "Editing Rackton code."
  :group 'languages
  :prefix "rackton-")

(defface rackton-constructor-face
  '((t :inherit font-lock-constant-face))
  "Face for Rackton data constructors."
  :group 'rackton)

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
  '("match" "match-let" "do" "let" "let*" "let+" "let&" "let%" "letrec"
    "lambda" "λ" "case-lambda" "case-λ" "cond" "if" "ann" "delay"
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

;;; Telling types and constructors apart
;;
;; Types and data constructors share one lexical shape, so the
;; classification is positional: a capitalized name is a TYPE when an
;; enclosing form puts it in type-level position (a `(: ...)'
;; signature, an arrow, a declaration head, a constructor's field, the
;; tail of a GADT clause, a #:deriving list, an export spec), and a
;; CONSTRUCTOR otherwise (expressions, match patterns, and the
;; constructor names a `data'/GADT declaration introduces).

(defconst rackton--type-form-heads
  '(: -> All foreign foreign-c define-alias
      data-out struct-out protocol-out)
  "Heads of forms whose every capitalized name is type-level.")

(defconst rackton--typed-head-forms
  '(data struct newtype class instance protocol racket)
  "Forms whose first argument is a type-level head.")

(defconst rackton--data-forms '(data struct newtype)
  "Declaration forms whose body introduces constructors.")

(defun rackton--symbol-at (pos)
  "Return the symbol starting at POS, or nil."
  (save-excursion
    (goto-char pos)
    (when (looking-at "\\(?:\\sw\\|\\s_\\)+")
      (intern-soft (match-string-no-properties 0)))))

(defun rackton--element-start (open n)
  "Start position of element N (0 = head) of the form opening at OPEN.
Return nil when the form has fewer elements."
  (save-excursion
    (goto-char (1+ open))
    (condition-case nil
        (progn
          (dotimes (_ n) (forward-sexp 1))
          (forward-sexp 1)
          (backward-sexp 1)
          (point))
      (scan-error nil))))

(defun rackton--colon-clause-p (open)
  "Non-nil when the form at OPEN has `:' as its second element.
That shape is a GADT constructor clause or a struct field."
  (let ((second (rackton--element-start open 1)))
    (and second (eq (rackton--symbol-at second) ':))))

(defun rackton--governing-keyword (open child)
  "The #:keyword governing CHILD inside the form at OPEN, or nil.
That is the nearest Racket keyword before CHILD at CHILD's own
nesting level, returned as a string."
  (save-excursion
    ;; Depth first: `syntax-ppss' moves point.
    (let ((depth (1+ (car (syntax-ppss open))))
          (found nil))
      (goto-char child)
      (while (and (not found)
                  (re-search-backward "#:\\(?:\\sw\\|\\s_\\)+" (1+ open) t))
        (when (= (car (syntax-ppss)) depth)
          (setq found (match-string-no-properties 0))))
      found)))

(defun rackton--type-position-p (pos)
  "Non-nil when the capitalized name at POS occupies a type position.
Walks the enclosing forms from the inside out; the first form that
determines type-ness or constructor-ness wins, and a name with no
deciding context is a constructor."
  (let ((opens (reverse (nth 9 (syntax-ppss pos)))) ; innermost first
        (child pos)        ; the element of the current form containing POS
        (inner pos)        ; the element one nesting level deeper than CHILD
        (decided nil))
    (while (and opens (not decided))
      (let* ((open (car opens))
             (head (rackton--symbol-at (1+ open))))
        (cond
         ((memq head rackton--type-form-heads)
          (setq decided 'type))
         ;; (Ctor : type ...) or [field : Type]
         ((rackton--colon-clause-p open)
          (setq decided (if (eq child (rackton--element-start open 0))
                            'constructor
                          'type)))
         ;; declaration head: (data (Maybe a) ...), (instance (Eq Box) ...)
         ((and (memq head rackton--typed-head-forms)
               (eq child (rackton--element-start open 1)))
          (setq decided 'type))
         ;; class/protocol keyword blocks: #:requires names constraints;
         ;; #:derive takes (SuperClass (define ...) ...) clauses whose
         ;; head names a superclass while the defines are expressions.
         ((memq head '(class protocol))
          (let ((governing (rackton--governing-keyword open child)))
            (cond ((equal governing "#:requires")
                   (setq decided 'type))
                  ((and (equal governing "#:derive")
                        (or (eq inner pos)
                            (eq pos (rackton--element-start inner 0))))
                   (setq decided 'type)))))
         ;; the rest of a data/struct/newtype body
         ((memq head rackton--data-forms)
          (setq decided
                (cond ((equal (rackton--governing-keyword open child)
                              "#:deriving")
                       'type)
                      ;; a bare nullary constructor, e.g. None
                      ((eq child pos) 'constructor)
                      ;; the head of a constructor clause, e.g. (Some a)
                      ((eq pos (rackton--element-start child 0)) 'constructor)
                      ;; a constructor's field, e.g. Integer in (EInt Integer)
                      (t 'type))))
         ;; (ann expr type) — type-level after the expression
         ((and (eq head 'ann)
               (not (eq child (rackton--element-start open 1))))
          (setq decided 'type)))
        (setq inner child
              child open
              opens (cdr opens))))
    (eq decided 'type)))

(defun rackton--search-capitalized (limit pred)
  "Find the next capitalized name before LIMIT whose start satisfies PRED.
Set the match data and leave point after the name; return non-nil when
found, as a font-lock matcher must."
  (let (found)
    (while (and (not found)
                (re-search-forward rackton--type-name-regexp limit t))
      ;; The predicate may move point (e.g. `syntax-ppss'); restore it
      ;; so the search always advances.
      (setq found (save-excursion
                    (save-match-data
                      (funcall pred (match-beginning 0))))))
    found))

(defun rackton--match-type-name (limit)
  "Font-lock matcher: the next type-position capitalized name before LIMIT."
  (rackton--search-capitalized limit #'rackton--type-position-p))

(defun rackton--match-constructor (limit)
  "Font-lock matcher: the next constructor-position name before LIMIT."
  (rackton--search-capitalized
   limit (lambda (pos) (not (rackton--type-position-p pos)))))

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
    ;; The name a define binds: (define (name ...) ...) or
    ;; (define name ...).  Stated here rather than inherited from
    ;; scheme-mode's keywords so buffers that only add these keywords
    ;; (the REPL) highlight it too.
    ("(define[ \t\n]+(?\\(\\(?:\\sw\\|\\s_\\)+\\)"
     (1 font-lock-function-name-face))
    ;; Racket keywords (#:deriving, #:derive, #:from, ...).  Stated
    ;; here rather than inherited from scheme-mode's keywords so
    ;; buffers that only add these keywords (the REPL) highlight them.
    ("#:\\(?:\\sw\\|\\s_\\)+" . font-lock-builtin-face)
    (rackton--match-type-name . font-lock-type-face)
    (rackton--match-constructor . 'rackton-constructor-face))
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
    (let&      . 1)
    (let%      . scheme-let-indent)     ; has a named variant, like let
    (λ         . 1)
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
