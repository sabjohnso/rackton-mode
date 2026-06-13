;;; rackton-repl-test.el --- Tests for rackton-repl  -*- lexical-binding: t; -*-

;;; Commentary:

;; Two tiers: unit tests for the pure pieces (form extraction, mode
;; derivation), and integration tests that drive a real
;; `racket -l rackton/repl' subprocess.  Integration tests skip when
;; Racket or the rackton package is not installed, so the suite still
;; passes on machines without the language.

;;; Code:

(require 'ert)
(require 'rackton-repl)

;;; Helpers

(defvar rackton-test--repl-available 'unknown
  "Cached result of the Racket/rackton availability probe.")

(defun rackton-test--repl-available-p ()
  "Non-nil when `racket -l rackton/repl' can actually run."
  (when (eq rackton-test--repl-available 'unknown)
    (setq rackton-test--repl-available
          (and (executable-find rackton-program)
               (zerop (call-process rackton-program nil nil nil
                                    "-e" "(require rackton/repl)")))))
  rackton-test--repl-available)

(defun rackton-test--wait-for (predicate timeout)
  "Wait up to TIMEOUT seconds for PREDICATE to return non-nil."
  (let ((deadline (+ (float-time) timeout)))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (accept-process-output (rackton-repl--process) 0.1))
    (funcall predicate)))

(defun rackton-test--ensure-repl ()
  "Start (or reuse) the shared test REPL; skip the test when unavailable."
  (unless (rackton-test--repl-available-p)
    (ert-skip "racket -l rackton/repl is not available"))
  (rackton-repl--ensure))

(defun rackton-test--repl-contains-p (text)
  "Non-nil when the REPL buffer contains TEXT."
  (with-current-buffer (rackton-repl--buffer)
    (save-excursion
      (goto-char (point-min))
      (search-forward text nil t))))

;;; Unit: mode and form extraction

(ert-deftest rackton-repl-mode-derives-from-comint ()
  (with-temp-buffer
    (inferior-rackton-mode)
    (should (derived-mode-p 'comint-mode))))

(ert-deftest rackton-repl-prompt-regexp-matches-prompt ()
  (should (string-match rackton-repl-prompt-regexp "λ> "))
  (should-not (string-match rackton-repl-prompt-regexp "lambda> ")))

(ert-deftest rackton-repl-region-forms-extracts-top-level-forms ()
  (with-temp-buffer
    (insert "#lang rackton\n"
            ";; a comment\n"
            "(define a 1)\n\n"
            "(define (b x)\n  x)\n")
    (rackton-mode)
    (should (equal (rackton-repl--region-forms (point-min) (point-max))
                   '("(define a 1)" "(define (b x)\n  x)")))))

;;; Unit: input ergonomics

(ert-deftest rackton-repl-strips-continuation-prompts ()
  (should (equal (rackton-repl--strip-continuations "..> ..> λ> ") "λ> "))
  (should (equal (rackton-repl--strip-continuations "..> sqr :: t\n")
                 "sqr :: t\n"))
  (should (equal (rackton-repl--strip-continuations "plain\n") "plain\n")))

(ert-deftest rackton-repl-input-complete-p-checks-balance ()
  (should (rackton-repl--input-complete-p "(sqr 2)"))
  (should (rackton-repl--input-complete-p "42"))
  (should (rackton-repl--input-complete-p "(match m\n  [(Some x) x])"))
  (should-not (rackton-repl--input-complete-p "(define (f x)"))
  (should-not (rackton-repl--input-complete-p "\"unterminated")))

(ert-deftest rackton-repl-buffer-fontifies-rackton-code ()
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert "λ> (define (eff x) (Some x))")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "define")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-keyword-face))
    (search-forward "eff")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-function-name-face))
    (search-forward "Some")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'rackton-constructor-face))
    ;; Racket keywords must highlight here too, not only in source
    ;; buffers (where scheme-mode's own rule covers them).
    (insert "\nλ> (struct P [x : Integer] #:deriving Eq)")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "#:deriving")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-builtin-face))))

(ert-deftest rackton-repl-submitted-input-keeps-fontification ()
  (rackton-test--ensure-repl)
  (with-current-buffer (rackton-repl--buffer)
    (goto-char (point-max))
    (insert "(define (eleventh x) x)")
    (font-lock-ensure)                  ; fontify the pending input
    (let ((pos (save-excursion (search-backward "define"))))
      (rackton-repl-return)
      ;; Sending must not strip the fontification the input had...
      (should (eq (get-text-property pos 'face)
                  'font-lock-keyword-face))
      ;; ...nor stamp it with comint's input face, which shadows the
      ;; `face' property wherever font-lock is live.
      (should-not (get-text-property pos 'font-lock-face)))))

;;; Integration: transport

(ert-deftest rackton-repl-starts-and-prompts ()
  (rackton-test--ensure-repl)
  (should (process-live-p (rackton-repl--process)))
  (with-current-buffer (rackton-repl--buffer)
    (should (derived-mode-p 'inferior-rackton-mode))))

(ert-deftest rackton-repl-evaluates-sent-forms ()
  (rackton-test--ensure-repl)
  (rackton-repl--send-form "(* 6 7)")
  (should (rackton-test--wait-for
           (lambda () (rackton-test--repl-contains-p "42 :: Integer"))
           15)))

(ert-deftest rackton-repl-return-sends-complete-input ()
  (rackton-test--ensure-repl)
  (with-current-buffer (rackton-repl--buffer)
    (goto-char (point-max))
    (insert "(* 5 5)")
    (rackton-repl-return))
  (should (rackton-test--wait-for
           (lambda () (rackton-test--repl-contains-p "25 :: Integer"))
           15)))

(ert-deftest rackton-repl-return-continues-incomplete-input ()
  (rackton-test--ensure-repl)
  (with-current-buffer (rackton-repl--buffer)
    (goto-char (point-max))
    (let ((input-start (process-mark (rackton-repl--process))))
      (unwind-protect
          (progn
            (insert "(define (ninth x)")
            (rackton-repl-return)
            ;; Not sent: the text still sits in the input region...
            (should (string-prefix-p "(define (ninth x)\n"
                                     (buffer-substring-no-properties
                                      input-start (point-max))))
            ;; ...and the new line is indented under the define, prompt
            ;; width included: "λ> (" puts the paren at column 3, body at 5.
            (should (= (current-column) 5)))
        ;; Clean the half-typed input up for the tests that follow.
        (delete-region input-start (point-max))))))

(ert-deftest rackton-repl-multiline-send-shows-no-continuation-prompts ()
  (rackton-test--ensure-repl)
  (rackton-repl--send-form "(define (tenth x)\n  (* 10 x))")
  (should (rackton-test--wait-for
           (lambda () (rackton-test--repl-contains-p "tenth ::"))
           15))
  (should-not (rackton-test--repl-contains-p "..>")))

;;; Integration: query channel

(ert-deftest rackton-repl-query-returns-type ()
  (rackton-test--ensure-repl)
  (let ((reply (rackton-repl-query ",type (lambda (x) x)")))
    (should (string-match-p "(-> a a)" reply))))

(ert-deftest rackton-repl-query-keeps-repl-buffer-clean ()
  (rackton-test--ensure-repl)
  (rackton-repl-query ",info Either")
  (should-not (rackton-test--repl-contains-p "Either (type ctor")))

;;; Integration: UI commands

(ert-deftest rackton-repl-eval-defun-defines-binding ()
  (rackton-test--ensure-repl)
  (with-temp-buffer
    (insert "(define (triple n) (* 3 n))")
    (rackton-mode)
    (goto-char (point-min))
    (rackton-eval-defun))
  (should (rackton-test--wait-for
           (lambda ()
             (string-match-p "(-> Integer Integer)"
                             (rackton-repl-query ",type triple")))
           15)))

(ert-deftest rackton-repl-describe-fills-doc-buffer ()
  (rackton-test--ensure-repl)
  (cl-letf (((symbol-function 'display-buffer) #'ignore))
    (rackton-describe-symbol "Maybe"))
  (with-current-buffer "*rackton-doc*"
    (save-excursion
      (goto-char (point-min))
      (should (search-forward "constructors:" nil t)))))

(ert-deftest rackton-repl-accepts-searches-by-type ()
  (rackton-test--ensure-repl)
  (cl-letf (((symbol-function 'display-buffer) #'ignore))
    (rackton-accepts "(List Integer)"))
  (with-current-buffer "*rackton-doc*"
    (save-excursion
      (goto-char (point-min))
      (should (search-forward "append" nil t)))))

;;; Integration: eldoc

(ert-deftest rackton-repl-eldoc-reports-type-of-symbol-at-point ()
  (rackton-test--ensure-repl)
  (rackton-repl-query "(define (quadruple n) (* 4 n))")
  (with-temp-buffer
    (insert "(quadruple 2)")
    (rackton-mode)
    (goto-char (+ (point-min) 3))      ; inside "quadruple"
    (let (reported)
      (rackton-repl-eldoc (lambda (doc &rest _) (setq reported doc)))
      (should reported)
      (should (string-match-p "::" reported)))))

(provide 'rackton-repl-test)
;;; rackton-repl-test.el ends here
