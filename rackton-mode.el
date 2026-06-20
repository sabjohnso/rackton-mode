;;; rackton-mode.el --- Major mode for the Rackton language  -*- lexical-binding: t; -*-

;; Author: Samuel B. Johnson <samuel.bryant.johnson@gmail.com>
;; Version: 0.4.25
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
(require 'imenu)
(require 'easymenu)

(defgroup rackton nil
  "Editing Rackton code."
  :group 'languages
  :prefix "rackton-")

(defcustom rackton-program "racket"
  "Program that hosts Rackton.
Used to launch the REPL, the LSP/debug servers, and the signature
search tool, each via a `-l rackton/...' module argument."
  :type 'string
  :group 'rackton)

(defcustom rackton-tab-always-indent 'complete
  "Value of `tab-always-indent' in Rackton buffers.
The default makes TAB indent the line and then complete the symbol at
point through the active completion-at-point backend (eglot's LSP
completion when connected, otherwise the REPL's).  Set to t for the
Lisp-traditional TAB that only ever indents."
  :type '(choice (const :tag "Indent, then complete" complete)
                 (const :tag "Indent only" t))
  :group 'rackton)

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
    "define-syntax" "define-syntax-rule"
    "type-family" "type-instance" "data-family" "data-instance"
    "define-constraint" "constraint-family"
    "instance" "protocol" "foreign" "foreign-c")
  "Forms that introduce definitions at the top of a Rackton module.")

(defconst rackton-expression-forms
  '("match" "match*" "match-let" "do" "let" "let*" "let+" "let&" "let%"
    "letrec" "lambda" "λ" "case-lambda" "case-λ" "cond" "if" "ann" "delay"
    "open" "handle" "escape" "proc" "rec" "feed" "update" "via" "racket")
  "Forms that head Rackton expressions.")

(defconst rackton-module-forms
  '("require" "only-in"
    "provide" "all-defined-out" "all-from-out" "data-out" "struct-out"
    "protocol-out" "rename-out" "except-out")
  "Module import/export forms and their spec sub-form introducers.
See the \"Module forms\" and \"provide-specs\" sections of the Rackton
reference.")

(defconst rackton-clause-keywords
  '("else")
  "Auxiliary keywords that head a clause but are not themselves forms.
`else' names the catch-all clause of `cond' and `case'.")

(defconst rackton-type-quantifiers
  '("All" "∀" "Exists")
  "The type quantifiers that head a type scheme.
`All' (or the mathematical `∀') is universal — (All (a) …) — and also
heads a protocol law; both spellings are surface synonyms (see
surface.rkt's `#:datum-literals (All ∀)').  `Exists' is its existential
dual — (Exists (a) …) — for first-class existential types.")

;;; Font-lock

(defconst rackton--quantifier-regexp
  (regexp-opt rackton-type-quantifiers 'symbols)
  "Match the type quantifier `All' or `∀' as a whole symbol.")

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
  '(: -> All Exists foreign foreign-c define-alias
      type-family type-instance data-family define-constraint constraint-family
      data-out struct-out protocol-out)
  "Heads of forms whose every capitalized name is type-level.
The family and constraint declarations live here because every name
they mention is a type, type constructor, or constraint — `data-instance'
is the lone exception, since it introduces value constructors.")

(defconst rackton--typed-head-forms
  '(data struct newtype data-instance instance protocol racket)
  "Forms whose first argument is a type-level head.")

(defconst rackton--data-forms '(data struct newtype data-instance)
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
         ;; protocol keyword blocks: #:requires names constraints;
         ;; #:derive takes (SuperClass (define ...) ...) clauses whose
         ;; head names a superclass while the defines are expressions.
         ((eq head 'protocol)
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
    ;; `else' heading a cond/case clause: [else ...] or (else ...).
    ;; It is a keyword only in that position, so anchor on the open
    ;; bracket rather than matching the bare symbol everywhere.
    (,(concat "[[(]\\(" (regexp-opt rackton-clause-keywords) "\\)\\_>")
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
    ;; The type quantifier `All'/`∀'.  Before the type-name rule so its
    ;; capitalized `All' reads as a keyword, not a type name.
    (,rackton--quantifier-regexp . font-lock-keyword-face)
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
    (match*    . 1)
    (match-let . 1)
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

;;; imenu
;;
;; A definition index for navigation: `define' bindings sit flat at the
;; top, and the type-, protocol-, and instance-introducing forms group
;; into submenus.  The forms are walked with the same element-position
;; helpers the font-lock classifier uses, so a name with nested types
;; (an `instance' head like (Eq (Maybe a))) is read correctly rather
;; than truncated by a paren-blind regexp.

(defconst rackton--imenu-type-heads
  '(data struct newtype define-alias define-effect)
  "Definition forms grouped under the imenu \"Types\" submenu.")

(defconst rackton--imenu-protocol-heads '(protocol)
  "Definition forms grouped under the imenu \"Protocols\" submenu.")

(defun rackton--form-name-bounds (open)
  "Buffer bounds (BEG . END) of the name the form at OPEN binds, or nil.
The name is the first symbol of element 1, so both (define (f x) …)
and (define f …) — and the parenthesised heads of `data', `protocol',
etc., and a (: name …) signature — resolve to the same token."
  (let ((e1 (rackton--element-start open 1)))
    (when e1
      (save-excursion
        (goto-char (if (eq (char-after e1) ?\() (1+ e1) e1))
        (when (looking-at "\\(?:\\sw\\|\\s_\\)+")
          (cons (match-beginning 0) (match-end 0)))))))

(defun rackton--form-bound-name (open)
  "The name the form at OPEN binds, as a string, or nil.
Read as buffer text rather than interned: definition names are
arbitrary identifiers, most of which are not symbols in the obarray, so
`intern-soft' would miss them.  See `rackton--form-name-bounds'."
  (let ((bounds (rackton--form-name-bounds open)))
    (when bounds
      (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun rackton--imenu-instance-label (open)
  "Label for the `instance' form at OPEN — its head with parens trimmed.
For example (instance (Eq (Maybe a)) …) yields \"Eq (Maybe a)\"."
  (let ((e1 (rackton--element-start open 1)))
    (when e1
      (save-excursion
        (goto-char e1)
        (let* ((end (progn (forward-sexp 1) (point)))
               (text (buffer-substring-no-properties e1 end)))
          (string-trim
           (replace-regexp-in-string
            "[ \t\n]+" " "
            (replace-regexp-in-string "\\`(\\|)\\'" "" text))))))))

(defun rackton--imenu-create-index ()
  "Build an imenu index of the buffer's Rackton definitions.
Top-level `define's appear flat; types, protocols, and instances are
grouped into submenus.  Only forms beginning at column zero are
indexed, so nested definitions are left out."
  (let ((functions '()) (types '()) (protocols '()) (instances '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^(" nil t)
        (let ((open (match-beginning 0)))
          ;; `syntax-ppss' and the element helpers move point; without
          ;; this `save-excursion' the next search would rematch the
          ;; same paren and the loop would never advance.
          (save-excursion
            (unless (nth 8 (syntax-ppss open))   ; not in a string or comment
              (let ((head (rackton--symbol-at (1+ open))))
                (cond
                 ((eq head 'define)
                  (when-let ((name (rackton--form-bound-name open)))
                    (push (cons name open) functions)))
                 ((memq head rackton--imenu-type-heads)
                  (when-let ((name (rackton--form-bound-name open)))
                    (push (cons name open) types)))
                 ((memq head rackton--imenu-protocol-heads)
                  (when-let ((name (rackton--form-bound-name open)))
                    (push (cons name open) protocols)))
                 ((eq head 'instance)
                  (when-let ((label (rackton--imenu-instance-label open)))
                    (push (cons label open) instances))))))))))
    (append (nreverse functions)
            (when types     (list (cons "Types" (nreverse types))))
            (when protocols (list (cons "Protocols" (nreverse protocols))))
            (when instances (list (cons "Instances" (nreverse instances)))))))

;;; Type annotations
;;
;; A definition's type signature is a sibling `(: name type)' form
;; sitting just above its `define'.  These helpers — pure source
;; structure, no type checker — locate the define enclosing point, read
;; an existing signature, and write one.  The type they place is the
;; caller's to supply, so the editing stays testable on its own.
;;
;; The type itself comes from a type source, named abstractly by
;; `rackton-type-functions'.  The LSP layer (rackton-lsp.el) and the
;; REPL layer (rackton-repl.el) each register one; the command prefers
;; whichever answers first, so a connected LSP needs no REPL and no
;; loaded source file.  With neither layer present the command says so.

(defvar rackton-type-functions nil
  "Abnormal hook naming the sources of a binding's type, tried in order.
Each function is called with the binding's NAME (a string) while point
is on that name, and returns the type expression as a string — the
`type' of a `(: name type)' signature, with no `name ::' prefix — or
nil when it cannot answer.  `rackton-annotate-definition' takes the
first non-nil reply.  The LSP layer registers its provider at the front
\(so an eglot connection is preferred) and the REPL layer appends
itself as a fallback.")

(defun rackton--enclosing-define (&optional pos)
  "Open-paren position of the nearest `define' form enclosing POS, or nil.
POS defaults to point.  Only the exact `define' head matches, not its
relatives (`define-alias', `define-effect', …)."
  (let ((open nil))
    ;; `nth 9' lists enclosing opens outermost-first; the last `define'
    ;; seen is therefore the innermost one enclosing POS.
    (dolist (o (nth 9 (syntax-ppss pos)) open)
      (when (eq (rackton--symbol-at (1+ o)) 'define)
        (setq open o)))))

(defun rackton--collapse-whitespace (string)
  "STRING with runs of whitespace collapsed to one space, trimmed.
So two type expressions are compared on structure, not on the line
breaks and indentation a printer or an author happened to use."
  (string-trim (replace-regexp-in-string "[ \t\n]+" " " string)))

(defun rackton--signature-type (open)
  "The type expression of the `(: name type)' signature at OPEN, normalized.
That is element 2 (the type), with whitespace collapsed; nil when the
form has no such element."
  (let ((type-start (rackton--element-start open 2)))
    (when type-start
      (rackton--collapse-whitespace
       (buffer-substring-no-properties type-start
                                       (scan-sexps type-start 1))))))

(defun rackton--preceding-signature (open name)
  "Bounds (BEG . END) of NAME's `(:' signature just above the define at OPEN.
The signature is the sibling form immediately preceding OPEN; the
result is nil unless that form is `(: NAME …)'.  BEG is its open paren,
END the position just after its close paren."
  (save-excursion
    (goto-char open)
    (condition-case nil
        (let ((sig-open (progn (backward-sexp 1) (point))))
          (when (and (eq (char-after sig-open) ?\()
                     (eq (rackton--symbol-at (1+ sig-open)) ':)
                     (equal (rackton--form-bound-name sig-open) name))
            (cons sig-open (scan-sexps sig-open 1))))
      (scan-error nil))))

(defun rackton--ensure-annotation (open name type)
  "Make NAME's signature above the define at OPEN read `(: NAME TYPE)'.
Insert the signature when absent, rewrite it when its type differs from
TYPE, and leave it untouched when it already agrees (whitespace aside).
Return `inserted', `updated', or `unchanged'."
  (let ((sig (rackton--preceding-signature open name))
        (desired (format "(: %s %s)" name type)))
    (cond
     ((null sig)
      (save-excursion
        ;; Insert at OPEN, then re-create the define's indentation after
        ;; the newline, so the signature lands on its own line above and
        ;; the define keeps its column.
        (let ((indent (make-string (progn (goto-char open) (current-column))
                                   ?\s)))
          (goto-char open)
          (insert desired "\n" indent)))
      'inserted)
     ((equal (rackton--signature-type (car sig))
             (rackton--collapse-whitespace type))
      'unchanged)
     (t
      (save-excursion
        (delete-region (car sig) (cdr sig))
        (goto-char (car sig))
        (insert desired))
      'updated))))

(defun rackton--scheme-type (line)
  "The type expression in a `name :: type' LINE, or nil.
Everything right of the first `::', whitespace collapsed, so a wrapped
multi-line scheme reads as the single type a signature needs.  A LINE
with no `::' — an error, or a hover for a protocol or type constructor —
yields nil.  Both the LSP hover and the REPL reply print this form, so
the reading is shared."
  (when (string-match "::" line)
    (let ((type (rackton--collapse-whitespace (substring line (match-end 0)))))
      (unless (string-empty-p type)
        type))))

(defun rackton-annotate-definition ()
  "Insert or correct the type signature for the define name at point.
Point must be on the name a `define' form binds.  Its type is read from
the first source in `rackton-type-functions' that can answer — an eglot
connection when present, otherwise a running REPL — and a `(: name
type)' signature is kept just above the define: inserted when absent,
rewritten when its type disagrees, and left untouched when it already
agrees.

With a Language Server connected (\\[eglot]) nothing more is needed.
Without one, start the REPL (\\[rackton-repl]) and evaluate the define
\(\\[rackton-eval-defun]) so its type can be inferred."
  (interactive)
  (let* ((open (rackton--enclosing-define))
         (bounds (and open (rackton--form-name-bounds open))))
    (unless (and bounds (<= (car bounds) (point)) (<= (point) (cdr bounds)))
      (user-error "Point is not on a `define'd name"))
    (let* ((name (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (type (run-hook-with-args-until-success 'rackton-type-functions name)))
      (unless type
        (user-error
         "No type for `%s' — connect the LSP (M-x eglot) or evaluate it in the REPL"
         name))
      (message "Annotation %s for `%s'"
               (rackton--ensure-annotation open name type) name))))

;;; Mode

;;;###autoload
(define-derived-mode rackton-mode scheme-mode "Rackton"
  "Major mode for editing Rackton code.

Rackton is a statically-typed functional language embedded in
Racket.  See the rackton repository's documentation for the
language itself."
  (setq-local lisp-indent-function #'rackton--indent-function)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-always-indent rackton-tab-always-indent)
  (setq-local imenu-create-index-function #'rackton--imenu-create-index)
  ;; Rackton, like Racket, is case-sensitive; scheme-mode's
  ;; font-lock-defaults set CASE-FOLD to t, which would make the
  ;; capitalized-name rule match every identifier.
  (let ((defaults (copy-sequence font-lock-defaults)))
    (setf (nth 2 defaults) nil)
    (setq-local font-lock-defaults defaults))
  (font-lock-add-keywords nil rackton-font-lock-keywords))

;;; Menu
;;
;; The base menu carries only what this layer owns: navigation by the
;; imenu index built above.  The optional REPL and search layers add
;; their own items ahead of "Go to Definition…" (see the
;; `easy-menu-add-item' calls in rackton-repl.el and rackton-search.el),
;; mirroring how each layer binds its own keys into `rackton-mode-map'.

(define-key rackton-mode-map (kbd "C-c :") #'rackton-annotate-definition)

(easy-menu-define rackton-mode-menu rackton-mode-map
  "Menu for `rackton-mode'."
  '("Rackton"
    ["Annotate Definition" rackton-annotate-definition
     :help "Insert or correct the type signature for the define at point"]
    ["Go to Definition…" imenu
     :help "Jump to a definition via the imenu index"]))

;;;###autoload
(add-to-list 'magic-mode-alist
             '("\\`#lang rackton\\(?:[[:space:]]\\|$\\)" . rackton-mode))

;; A file already starting with `#lang rackton' is recognized at open
;; time by the `magic-mode-alist' entry above.  A *new* .rkt file is
;; empty when first visited, so it opens in the .rkt default mode (often
;; `scheme-mode') before its `#lang' line is typed.  The watcher below
;; upgrades such a buffer once the line is written.

(defconst rackton--lang-line-regexp "\\`#lang rackton\\(?:[[:space:]]\\|$\\)"
  "Match a buffer beginning with a `#lang rackton' line.
Kept identical to the `magic-mode-alist' entry above; that entry holds
its own literal because the autoload cookie copies it verbatim, before
this constant exists.")

(defun rackton--enable-on-lang-line ()
  "Switch to `rackton-mode' when the first line reads `#lang rackton'.
A no-op once already in `rackton-mode', or when the first line is some
other `#lang'.  Turning the mode on kills buffer-local variables, so a
watcher installed by `rackton--watch-lang-line' removes itself here."
  (when (and (not (derived-mode-p 'rackton-mode))
             (save-excursion
               (goto-char (point-min))
               (looking-at-p rackton--lang-line-regexp)))
    (rackton-mode)))

(defun rackton--watch-lang-line ()
  "Watch a .rkt buffer that opened in another mode for a `#lang rackton' line.
For `find-file-hook': on a `.rkt' file not already in `rackton-mode',
re-check the first line after each self-inserted character so typing the
`#lang rackton' line into a fresh file switches the mode (see
`rackton--enable-on-lang-line')."
  (when (and buffer-file-name
             (string-suffix-p ".rkt" buffer-file-name)
             (not (derived-mode-p 'rackton-mode)))
    (add-hook 'post-self-insert-hook #'rackton--enable-on-lang-line nil t)))

;;; Paredit: structural braces for map/set literals
;;
;; Rackton writes map literals as `{..}' and set literals as `#{..}',
;; so braces are balanced delimiters just like `()' and `[]'.  The
;; syntax table already says so, which is enough for paredit's
;; navigation and slurp/barf.  Paredit only declines to *bind* the
;; brace keys by default, leaving that opt-in to the user.  This
;; command supplies the binding, on request, without making paredit a
;; dependency of the mode.

(defvar paredit-mode-map)               ; defined by paredit when it loads
(declare-function paredit-open-curly "paredit")
(declare-function paredit-close-curly "paredit")
(declare-function paredit-wrap-curly "paredit")

(defun rackton--bind-paredit-curly (map)
  "Bind the curly-brace structural-editing keys in MAP, and return MAP.
`{' inserts a balanced pair, `}' moves past the close, and \\`M-{'
wraps the following form — the brace analogues of paredit's round and
square keys.  The commands are named symbolically, so MAP needs no
loaded paredit and the binding is idempotent."
  (define-key map "{" #'paredit-open-curly)
  (define-key map "}" #'paredit-close-curly)
  (define-key map (kbd "M-{") #'paredit-wrap-curly)
  map)

;;;###autoload
(defun rackton-enable-paredit-curly ()
  "Make paredit treat `{' and `}' like `(' `)' and `[' `]'.
Rackton's map (`{..}') and set (`#{..}') literals are balanced
brace forms, but paredit leaves the brace keys unbound by default.
This binds them in `paredit-mode-map' — `{'/`}' to paredit's curly
insert and close commands and \\`M-{' to `paredit-wrap-curly', mirroring
the round and square bindings — so the change applies wherever paredit
is active.  Opt-in: call it from your init or with \\[execute-extended-command].
Signals a `user-error' when paredit is not installed."
  (interactive)
  (unless (require 'paredit nil t)
    (user-error "Paredit is not installed"))
  (rackton--bind-paredit-curly paredit-mode-map))

(provide 'rackton-mode)
;;; rackton-mode.el ends here
