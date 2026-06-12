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

;;; Font-lock

(ert-deftest rackton-mode-fontifies-data-keyword ()
  (should (rackton-test--has-face-p
           "(data (Maybe a) None (Some a))"
           "data" 'font-lock-keyword-face)))

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

(ert-deftest rackton-mode-fontifies-type-signature-form ()
  (let ((code "(: parse (-> SExpr (Result Expr)))"))
    (should (rackton-test--has-face-p code ":" 'font-lock-keyword-face))
    (should (rackton-test--has-face-p code "parse"
                                      'font-lock-function-name-face))))

(ert-deftest rackton-mode-fontifies-capitalized-names-as-types ()
  (should (rackton-test--has-face-p
           "(data (Maybe a) None (Some a))"
           "Maybe" 'font-lock-type-face))
  (should (rackton-test--has-face-p
           "(from-maybe 0 (Some 7))"
           "Some" 'font-lock-type-face)))

(ert-deftest rackton-mode-leaves-lowercase-identifiers-plain ()
  (should-not (rackton-test--face-at "(from-maybe d m)" "from-maybe")))

(ert-deftest rackton-mode-fontifies-defined-name ()
  (should (rackton-test--has-face-p
           "(define (from-maybe d m) d)"
           "from-maybe" 'font-lock-function-name-face)))

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

(provide 'rackton-mode-test)
;;; rackton-mode-test.el ends here
