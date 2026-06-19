;;; rackton-mode-test.el --- Tests for rackton-mode  -*- lexical-binding: t; -*-

;;; Commentary:

;; Behavioral tests for rackton-mode: mode selection, font-lock, and
;; indentation.  Each test exercises the mode through its public
;; interface (a buffer in `rackton-mode'), never through internals, so
;; the implementation is free to change shape under the tests.

;;; Code:

(require 'ert)
(require 'rackton-mode)

;;; Helpers

(defun rackton-test--face-at (code target)
  "Fontify CODE in `rackton-mode'; return face at start of first TARGET."
  (with-temp-buffer
    (insert code)
    (rackton-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward target)
    (get-text-property (match-beginning 0) 'face)))

(defun rackton-test--has-face-p (code target face)
  "Non-nil when first TARGET in CODE is fontified with FACE."
  (let ((found (rackton-test--face-at code target)))
    (memq face (if (listp found) found (list found)))))

(defun rackton-test--reindent (code)
  "Strip CODE's leading whitespace per line, reindent in `rackton-mode'."
  (with-temp-buffer
    (insert code)
    (rackton-mode)
    (goto-char (point-min))
    (while (not (eobp))
      (skip-chars-forward " \t")
      (delete-region (line-beginning-position) (point))
      (forward-line 1))
    (indent-region (point-min) (point-max))
    (buffer-string)))

(defmacro rackton-test--indents-to (code)
  "Assert that CODE is exactly what rackton-mode reindents it to."
  `(should (equal (rackton-test--reindent ,code) ,code)))

(defun rackton-test--rackton-menu ()
  "The \"Rackton\" menu keymap from `rackton-mode-map', or nil.
Found by display name so the test does not depend on the event
symbol easy-menu derives from it."
  (let ((menubar (lookup-key rackton-mode-map [menu-bar]))
        found)
    (when (keymapp menubar)
      (map-keymap
       (lambda (_event binding)
         (when (and (not found)
                    (eq (car-safe binding) 'menu-item)
                    (equal (nth 1 binding) "Rackton"))
           (setq found (nth 2 binding))))
       menubar))
    found))

(defun rackton-test--menu-commands (keymap)
  "All command symbols reachable in menu KEYMAP, descending submenus."
  (let (cmds)
    (map-keymap
     (lambda (_event binding)
       (let ((def (cond ((eq (car-safe binding) 'menu-item) (nth 2 binding))
                        ((consp binding) (cdr binding))
                        (t binding))))
         (cond ((keymapp def)
                (setq cmds (append (rackton-test--menu-commands def) cmds)))
               ((commandp def)
                (push def cmds)))))
     keymap)
    cmds))

;;; Mode selection

(ert-deftest rackton-mode-derives-from-scheme-mode ()
  (with-temp-buffer
    (rackton-mode)
    (should (derived-mode-p 'scheme-mode))
    (should (string-prefix-p ";" comment-start))))

(ert-deftest rackton-mode-detected-from-lang-line ()
  (with-temp-buffer
    (insert "#lang rackton\n(define (f x) x)\n")
    (set-auto-mode)
    (should (eq major-mode 'rackton-mode))))

(ert-deftest rackton-mode-not-applied-to-other-langs ()
  (with-temp-buffer
    (insert "#lang racket/base\n(define (f x) x)\n")
    (set-auto-mode)
    (should-not (eq major-mode 'rackton-mode))))

;;; Mode selection: switching once the lang line is typed
;;
;; A new, empty .rkt file opens in another mode (the .rkt default) before
;; its `#lang' line exists.  `rackton--enable-on-lang-line' upgrades it
;; once that line reads `#lang rackton'.

(ert-deftest rackton-enable-switches-on-lang-line ()
  "A non-rackton buffer whose first line is `#lang rackton' switches."
  (with-temp-buffer
    (scheme-mode)
    (insert "#lang rackton\n(provide (data-out T))\n")
    (rackton--enable-on-lang-line)
    (should (derived-mode-p 'rackton-mode))))

(ert-deftest rackton-enable-leaves-other-langs ()
  "A different `#lang' line is left in its original mode."
  (with-temp-buffer
    (scheme-mode)
    (insert "#lang racket/base\n(define (f x) x)\n")
    (rackton--enable-on-lang-line)
    (should (eq major-mode 'scheme-mode))))

(ert-deftest rackton-enable-noop-when-already-rackton ()
  "Already in `rackton-mode', the check does nothing untoward."
  (with-temp-buffer
    (insert "#lang rackton\n")
    (rackton-mode)
    (rackton--enable-on-lang-line)
    (should (derived-mode-p 'rackton-mode))))

(ert-deftest rackton-watch-switches-as-lang-line-is-typed ()
  "Typing the `#lang rackton' line into a watched .rkt buffer switches it.
Mimics `find-file-hook': set a .rkt name, land in scheme-mode, install
the watcher, then type the line a character at a time (each through
`self-insert-command', which runs `post-self-insert-hook')."
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "scratch-new.rkt"
                                             temporary-file-directory))
    (unwind-protect
        (progn
          (scheme-mode)
          (rackton--watch-lang-line)
          (should (eq major-mode 'scheme-mode)) ; empty file: not yet rackton
          (dolist (ch (append "#lang rackton" nil))
            (setq last-command-event ch)
            (self-insert-command 1))
          (should (derived-mode-p 'rackton-mode)))
      (set-buffer-modified-p nil))))

(ert-deftest rackton-watch-skips-non-rkt-files ()
  "The watcher installs nothing for a non-.rkt buffer."
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "notes.txt"
                                             temporary-file-directory))
    (scheme-mode)
    (rackton--watch-lang-line)
    (should-not (memq #'rackton--enable-on-lang-line post-self-insert-hook))))

;;; Font-lock

(ert-deftest rackton-mode-fontifies-data-keyword ()
  (should (rackton-test--has-face-p
           "(data (Maybe a) None (Some a))"
           "data" 'font-lock-keyword-face)))

(ert-deftest rackton-mode-fontifies-define-syntax-keywords ()
  ;; The macro-defining forms `define-syntax' and `define-syntax-rule'
  ;; head a definition and read as keywords, like the other define forms.
  (should (rackton-test--has-face-p
           "(define-syntax (twice stx) stx)"
           "define-syntax" 'font-lock-keyword-face))
  (should (rackton-test--has-face-p
           "(define-syntax-rule (twice x) (+ x x))"
           "define-syntax-rule" 'font-lock-keyword-face)))

(ert-deftest rackton-mode-fontifies-class-and-instance-keywords ()
  (should (rackton-test--has-face-p
           "(class (Functor f)\n  (: fmap (-> (-> a b) (-> (f a) (f b)))))"
           "class" 'font-lock-keyword-face))
  (should (rackton-test--has-face-p
           "(instance (Functor Box)\n  (define (fmap f b) b))"
           "instance" 'font-lock-keyword-face)))

(ert-deftest rackton-mode-fontifies-match-and-do-keywords ()
  (should (rackton-test--has-face-p
           "(match m\n  [(None) 0])"
           "match" 'font-lock-keyword-face))
  (should (rackton-test--has-face-p
           "(do [x <- m]\n  (pure x))"
           "do" 'font-lock-keyword-face)))

(ert-deftest rackton-mode-fontifies-cond-else ()
  ;; `else' heads the catch-all clause of cond/case; it reads as a
  ;; keyword there, not as a value.
  (should (rackton-test--has-face-p
           "(cond [(< n 0) \"negative\"]\n      [(> n 0) \"positive\"]\n      [else \"neutral\"])"
           "else" 'font-lock-keyword-face)))

(ert-deftest rackton-mode-fontifies-forall-quantifier ()
  ;; `All' and its synonym `∀' quantify a type scheme; both read as
  ;; keywords, not as the type names their capitalization would suggest.
  (should (rackton-test--has-face-p
           "(: id (All (a) (-> a a)))" "All" 'font-lock-keyword-face))
  (should (rackton-test--has-face-p
           "(: id (∀ (a) (-> a a)))" "∀" 'font-lock-keyword-face)))

(ert-deftest rackton-mode-fontifies-type-signature-form ()
  (let ((code "(: parse (-> SExpr (Result Expr)))"))
    (should (rackton-test--has-face-p code ":" 'font-lock-keyword-face))
    (should (rackton-test--has-face-p code "parse"
                                      'font-lock-function-name-face))))

(ert-deftest rackton-mode-fontifies-constructors-in-expressions ()
  (should (rackton-test--has-face-p
           "(from-maybe 0 (Some 7))"
           "Some" 'rackton-constructor-face))
  (should (rackton-test--has-face-p
           "(fold-left max 0 Nil)"
           "Nil" 'rackton-constructor-face)))

(ert-deftest rackton-mode-fontifies-constructors-in-patterns ()
  (let ((code "(match m\n  [(Some x) x]\n  [None 0])"))
    (should (rackton-test--has-face-p code "Some" 'rackton-constructor-face))
    (should (rackton-test--has-face-p code "None" 'rackton-constructor-face))))

(ert-deftest rackton-mode-fontifies-data-declarations ()
  (let ((code "(data (Maybe a) None (Some a))"))
    (should (rackton-test--has-face-p code "Maybe" 'font-lock-type-face))
    (should (rackton-test--has-face-p code "None" 'rackton-constructor-face))
    (should (rackton-test--has-face-p code "Some" 'rackton-constructor-face)))
  ;; constructor fields are types
  (let ((code "(data Expr (EInt Integer))"))
    (should (rackton-test--has-face-p code "EInt" 'rackton-constructor-face))
    (should (rackton-test--has-face-p code "Integer" 'font-lock-type-face))))

(ert-deftest rackton-mode-fontifies-gadt-clauses ()
  (let ((code "(data (Tagged a)\n  (IntTag : (Tagged Integer)))"))
    (should (rackton-test--has-face-p code "Tagged" 'font-lock-type-face))
    (should (rackton-test--has-face-p code "IntTag" 'rackton-constructor-face))
    (should (rackton-test--has-face-p code "Tagged Integer"
                                      'font-lock-type-face))
    (should (rackton-test--has-face-p code "Integer" 'font-lock-type-face))))

(ert-deftest rackton-mode-fontifies-struct-declarations ()
  (let ((code "(struct Point\n  [x : Integer]\n  [y : Integer]\n  #:deriving Eq Show)"))
    (should (rackton-test--has-face-p code "Point" 'font-lock-type-face))
    (should (rackton-test--has-face-p code "Integer" 'font-lock-type-face))
    (should (rackton-test--has-face-p code "Eq" 'font-lock-type-face))
    (should (rackton-test--has-face-p code "Show" 'font-lock-type-face))))

(ert-deftest rackton-mode-fontifies-signature-types ()
  (let ((code "(: parse (-> SExpr (Result Expr)))"))
    (should (rackton-test--has-face-p code "SExpr" 'font-lock-type-face))
    (should (rackton-test--has-face-p code "Result" 'font-lock-type-face))))

(ert-deftest rackton-mode-fontifies-instance-heads-as-types ()
  ;; the head names types; default-method bodies are expressions
  (let ((code "(instance (Functor Box)\n  (define (fmap f b) (MkBox (f b))))"))
    (should (rackton-test--has-face-p code "Functor" 'font-lock-type-face))
    (should (rackton-test--has-face-p code "Box" 'font-lock-type-face))
    (should (rackton-test--has-face-p code "MkBox" 'rackton-constructor-face))))

(ert-deftest rackton-mode-fontifies-ann-positions ()
  (let ((code "(ann (Some 1) (Maybe Integer))"))
    (should (rackton-test--has-face-p code "Some" 'rackton-constructor-face))
    (should (rackton-test--has-face-p code "Maybe" 'font-lock-type-face))))

(ert-deftest rackton-mode-fontifies-racket-keywords ()
  (should (rackton-test--has-face-p
           "(struct P [x : Integer] #:deriving Eq)"
           "#:deriving" 'font-lock-builtin-face))
  (should (rackton-test--has-face-p
           "(protocol (C w)\n  #:derive\n  ((Semigroup (define (mappend a b) a))))"
           "#:derive" 'font-lock-builtin-face)))

(ert-deftest rackton-mode-classifies-protocol-keyword-blocks ()
  (let ((code (concat "(protocol (Stack (s => Eq))\n"
                      "  (: push (-> a (s a) (s a)))\n"
                      "  #:requires (Show s)\n"
                      "  #:derive\n"
                      "  ((Semigroup\n"
                      "    (define (mappend a b) (Combine a b)))))")))
    ;; superclass bound in the head
    (should (rackton-test--has-face-p code "Eq" 'font-lock-type-face))
    ;; constraint after #:requires
    (should (rackton-test--has-face-p code "Show" 'font-lock-type-face))
    ;; the superclass a derivation clause names
    (should (rackton-test--has-face-p code "Semigroup" 'font-lock-type-face))
    ;; ...but its method bodies are ordinary expressions
    (should (rackton-test--has-face-p code "Combine"
                                      'rackton-constructor-face))))

(ert-deftest rackton-mode-fontifies-export-spec-names-as-types ()
  (let ((code "(provide (data-out Maybe) (struct-out Point))"))
    (should (rackton-test--has-face-p code "Maybe" 'font-lock-type-face))
    (should (rackton-test--has-face-p code "Point" 'font-lock-type-face))))

(ert-deftest rackton-mode-leaves-lowercase-identifiers-plain ()
  (should-not (rackton-test--face-at "(from-maybe d m)" "from-maybe")))

(ert-deftest rackton-mode-fontifies-defined-name ()
  (should (rackton-test--has-face-p
           "(define (from-maybe d m) d)"
           "from-maybe" 'font-lock-function-name-face)))

(ert-deftest rackton-mode-fontifies-lambda-aliases-and-monadic-lets ()
  (should (rackton-test--has-face-p
           "(λ (x) (* x x))" "λ" 'font-lock-keyword-face))
  (should (rackton-test--has-face-p
           "(case-λ [(x) x])" "case-λ" 'font-lock-keyword-face))
  (should (rackton-test--has-face-p
           "(let& ([a ma]) a)" "let&" 'font-lock-keyword-face))
  (should (rackton-test--has-face-p
           "(let% ([a ma]) a)" "let%" 'font-lock-keyword-face)))

(ert-deftest rackton-mode-indents-lambda-alias-and-monadic-lets ()
  (rackton-test--indents-to "(λ (x)\n  (* x x))")
  (rackton-test--indents-to
   "(let& ([a ma]\n       [b mb])\n  (pure (+ a b)))")
  ;; let% has a named (loop) variant, indented like named let
  (rackton-test--indents-to
   "(let% loop ([a ma])\n  body)"))

(ert-deftest rackton-mode-fontifies-module-forms ()
  (let ((code (concat "(require rackton/data/list (only-in m f))\n"
                      "(provide (all-defined-out)\n"
                      "         (all-from-out m)\n"
                      "         (data-out Maybe)\n"
                      "         (struct-out Point)\n"
                      "         (protocol-out Stack)\n"
                      "         (rename-out [f g])\n"
                      "         (except-out (all-from-out m) f))")))
    (dolist (form '("require" "only-in" "provide" "all-defined-out"
                    "all-from-out" "data-out" "struct-out" "protocol-out"
                    "rename-out" "except-out"))
      (should (rackton-test--has-face-p code form 'font-lock-keyword-face)))))

(ert-deftest rackton-mode-keywords-in-strings-stay-strings ()
  (should (rackton-test--has-face-p
           "(println \"data and match walk in\")"
           "data" 'font-lock-string-face)))

;;; Indentation

(ert-deftest rackton-mode-indents-match-clauses ()
  (rackton-test--indents-to
   "(match s\n  [(SInt n) (Ok n)]\n  [(SSym x) (Ok x)])"))

(ert-deftest rackton-mode-indents-instance-body ()
  (rackton-test--indents-to
   "(instance (Functor Box)\n  (define (fmap f b) b))"))

(ert-deftest rackton-mode-indents-class-body ()
  (rackton-test--indents-to
   "(class (Functor f)\n  (: fmap (-> (-> a b) (-> (f a) (f b)))))"))

(ert-deftest rackton-mode-indents-data-constructors ()
  (rackton-test--indents-to
   "(data Expr\n  (EInt Integer)\n  (EVar String))"))

(ert-deftest rackton-mode-indents-do-house-style ()
  ;; Binding clauses align under the first binding; the trailing body
  ;; expression sits at plain body indent.  This is the style used
  ;; throughout the rackton repo (see examples/todo.rkt).
  (rackton-test--indents-to
   "(do [path  <- todo-file]\n    [items <- (read-items path)]\n  (print-items items 1))"))

(ert-deftest rackton-mode-indents-cond-clauses ()
  (rackton-test--indents-to
   "(cond\n  [(symbol? x) (SSym x)]\n  [else (panic \"bad\")])"))

(ert-deftest rackton-mode-indents-racket-escape-body ()
  ;; (racket type (var ...) body ...) — type and captures are
  ;; distinguished arguments; the body sits at body indent.
  (rackton-test--indents-to
   "(racket String (name)\n  (string-append \"hello \" name))"))

(ert-deftest rackton-mode-indents-describe-body ()
  ;; rackton/unit test suites: (describe "name" test ...)
  (rackton-test--indents-to
   "(describe \"checks\"\n  (it \"arithmetic\" (check-equal? (+ 2 2) 4)))"))

(ert-deftest rackton-mode-keeps-scheme-indentation-for-plain-forms ()
  (rackton-test--indents-to "(let ((x 1))\n  x)")
  (rackton-test--indents-to "(println (mappend a\n                  b))"))

;;; imenu

(defmacro rackton-test--with-imenu (code &rest body)
  "Build the imenu index for CODE in `rackton-mode'; eval BODY with it.
INDEX is bound to the index alist; point may be moved freely."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,code)
     (rackton-mode)
     (let ((index (funcall imenu-create-index-function)))
       ,@body)))

(ert-deftest rackton-mode-imenu-lists-functions-flat ()
  (rackton-test--with-imenu "(define (foo x) x)\n(define bar 1)\n"
    (should (assoc "foo" index))
    (should (assoc "bar" index))
    (goto-char (cdr (assoc "foo" index)))
    (should (looking-at "(define (foo"))
    (goto-char (cdr (assoc "bar" index)))
    (should (looking-at "(define bar"))))

(ert-deftest rackton-mode-imenu-groups-types ()
  (rackton-test--with-imenu
      (concat "(data (Maybe a) None (Some a))\n"
              "(struct Point [x : Integer])\n"
              "(newtype Age Integer)\n"
              "(define-alias (Endo a) (-> a a))\n"
              "(define-effect Counter)\n")
    (let ((types (cdr (assoc "Types" index))))
      (should (assoc "Maybe" types))
      (should (assoc "Point" types))
      (should (assoc "Age" types))
      (should (assoc "Endo" types))
      (should (assoc "Counter" types)))))

(ert-deftest rackton-mode-imenu-groups-classes ()
  (rackton-test--with-imenu
      (concat "(class (Functor f)\n  (: fmap (-> (-> a b) (-> (f a) (f b)))))\n"
              "(protocol (Stack (s => Eq))\n  (: push (-> a (s a) (s a))))\n")
    (let ((classes (cdr (assoc "Classes" index))))
      (should (assoc "Functor" classes))
      (should (assoc "Stack" classes)))))

(ert-deftest rackton-mode-imenu-labels-instances-by-head ()
  (rackton-test--with-imenu
      (concat "(instance (Functor Box)\n  (define (fmap f b) b))\n"
              "(instance (Eq (Maybe a))\n  (define (== x y) #t))\n")
    (let ((instances (cdr (assoc "Instances" index))))
      ;; the whole instance head is the label, nested types included
      (should (assoc "Functor Box" instances))
      (should (assoc "Eq (Maybe a)" instances)))))

(ert-deftest rackton-mode-imenu-skips-nested-and-signatures ()
  (rackton-test--with-imenu
      "(: foo (-> Integer Integer))\n(define (foo x)\n  (define helper 1)\n  x)\n"
    (should (assoc "foo" index))         ; the top-level define
    (should-not (assoc "helper" index))  ; nested define is not indexed
    ;; the (: ...) signature adds no second "foo" entry
    (should (= 1 (length (seq-filter (lambda (e) (equal (car e) "foo"))
                                     index))))))

;;; Type annotations

(ert-deftest rackton-scheme-type-extracts-type ()
  "The type is everything right of `::', with whitespace collapsed."
  (should (equal (rackton--scheme-type "foo :: Integer") "Integer"))
  (should (equal (rackton--scheme-type
                  "foo :: (All\n          (a)\n          (-> a a))")
                 "(All (a) (-> a a))"))
  ;; A hover with no `::' (a protocol or type constructor) yields nil.
  (should-not (rackton--scheme-type "Foo — protocol")))

(defmacro rackton-test--annotating (code type &rest body)
  "Insert CODE in a `rackton-mode' buffer; bind a type source yielding TYPE.
`rackton-type-functions' is stubbed to a lone provider returning TYPE
(or nil), so the command is exercised independently of any backend.
Point starts at `point-min'."
  (declare (indent 2))
  `(let ((rackton-type-functions (list (lambda (_name) ,type))))
     (with-temp-buffer
       (insert ,code)
       (rackton-mode)
       (goto-char (point-min))
       ,@body)))

(ert-deftest rackton-annotate-inserts-when-absent ()
  "With no signature above, one is inserted matching the reported type."
  (rackton-test--annotating
      "(define (sqr x) (* x x))\n" "(-> Integer Integer)"
    (search-forward "(define (")        ; point on the bound name `sqr'
    (rackton-annotate-definition)
    (should (equal (buffer-string)
                   "(: sqr (-> Integer Integer))\n(define (sqr x) (* x x))\n"))))

(ert-deftest rackton-annotate-fixes-wrong-type ()
  "An existing signature whose type disagrees is rewritten."
  (rackton-test--annotating
      "(: sqr (-> String String))\n(define (sqr x) (* x x))\n"
      "(-> Integer Integer)"
    (search-forward "(define (")
    (rackton-annotate-definition)
    (should (equal (buffer-string)
                   "(: sqr (-> Integer Integer))\n(define (sqr x) (* x x))\n"))))

(ert-deftest rackton-annotate-leaves-correct-alone ()
  "A signature already agreeing with the reported type is untouched."
  (rackton-test--annotating
      "(: sqr (-> Integer Integer))\n(define (sqr x) (* x x))\n"
      "(-> Integer Integer)"
    (search-forward "(define (")
    (let ((before (buffer-string)))
      (rackton-annotate-definition)
      (should (equal (buffer-string) before)))))

(ert-deftest rackton-annotate-ignores-whitespace-differences ()
  "A signature differing only in whitespace counts as correct, so untouched."
  (rackton-test--annotating
      "(: sqr (->  Integer\n            Integer))\n(define (sqr x) (* x x))\n"
      "(-> Integer Integer)"
    (search-forward "(define (")
    (let ((before (buffer-string)))
      (rackton-annotate-definition)
      (should (equal (buffer-string) before)))))

(ert-deftest rackton-annotate-handles-value-define ()
  "A (define name value) form, not just a function, gets a signature."
  (rackton-test--annotating
      "(define answer 42)\n" "Integer"
    (search-forward "(define ")          ; point on `answer'
    (rackton-annotate-definition)
    (should (equal (buffer-string)
                   "(: answer Integer)\n(define answer 42)\n"))))

(ert-deftest rackton-annotate-preserves-indentation ()
  "The inserted signature is indented to match the define it heads."
  (rackton-test--annotating
      "  (define (sqr x) (* x x))\n" "(-> Integer Integer)"
    (search-forward "(define (")
    (rackton-annotate-definition)
    (should (equal (buffer-string)
                   "  (: sqr (-> Integer Integer))\n  (define (sqr x) (* x x))\n"))))

(ert-deftest rackton-annotate-requires-point-on-name ()
  "Point off the bound name (in the body) is a `user-error', no edit."
  (rackton-test--annotating
      "(define (sqr x) (* x x))\n" "(-> Integer Integer)"
    (search-forward "* x")               ; in the body, not on `sqr'
    (should-error (rackton-annotate-definition) :type 'user-error)))

(ert-deftest rackton-annotate-errors-without-a-type ()
  "When no type source can answer, the command errors rather than editing."
  (rackton-test--annotating
      "(define (sqr x) (* x x))\n" nil     ; provider yields nil
    (search-forward "(define (")
    (should-error (rackton-annotate-definition) :type 'user-error)))

;;; menu

(ert-deftest rackton-mode-defines-menu ()
  "`rackton-mode-map' carries a \"Rackton\" menu-bar menu."
  (should (keymapp (rackton-test--rackton-menu))))

(ert-deftest rackton-mode-menu-offers-imenu-navigation ()
  "The base menu offers imenu-based navigation."
  (should (memq 'imenu
                (rackton-test--menu-commands (rackton-test--rackton-menu)))))

(provide 'rackton-mode-test)
;;; rackton-mode-test.el ends here
