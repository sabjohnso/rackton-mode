;;; rackton-repl-test.el --- Tests for rackton-repl  -*- lexical-binding: t; -*-

;;; Commentary:

;; Two tiers: unit tests for the pure pieces (form extraction, mode
;; derivation), and integration tests that drive a real
;; `racket -l rackton/repl' subprocess.  Integration tests skip when
;; Racket or the rackton package is not installed, so the suite still
;; passes on machines without the language.

;;; Code:

(require 'ert)
(require 'cl-lib)
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

(ert-deftest rackton-repl-restart-preserves-minor-modes ()
  "Restarting a killed REPL reuses its buffer without re-running the
major mode, so a minor mode the user turned on (e.g. paredit) is not
swept away by `kill-all-local-variables'."
  (let ((buf (get-buffer-create rackton-repl--buffer-name)))
    (unwind-protect
        (progn
          ;; The buffer a `,q' leaves behind: set up as a REPL, no process,
          ;; with a buffer-local minor mode the user enabled by some means
          ;; other than the mode hook.
          (with-current-buffer buf
            (inferior-rackton-mode)
            (visual-line-mode 1))
          ;; Stub the subprocess machinery so the restart needs no Racket.
          (cl-letf (((symbol-function 'make-comint-in-buffer)
                     (lambda (&rest _) buf))
                    ((symbol-function 'rackton-repl--wait-for-prompt)
                     #'ignore))
            (rackton-repl--ensure))
          (with-current-buffer buf
            (should (derived-mode-p 'inferior-rackton-mode))
            (should (bound-and-true-p visual-line-mode))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

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

(ert-deftest rackton-repl-blank-line-precedes-each-prompt ()
  ;; A prompt at a line start gains a blank line before it.
  (should (equal (rackton-repl--blank-before-prompts "42 :: Integer\nλ> ")
                 "42 :: Integer\n\nλ> "))
  ;; A chunk that is only a prompt gains a leading newline (a blank
  ;; line, since the buffer before it already ends in one).
  (should (equal (rackton-repl--blank-before-prompts "λ> ") "\nλ> "))
  ;; Output with no prompt is untouched.
  (should (equal (rackton-repl--blank-before-prompts "no prompt\n")
                 "no prompt\n")))

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

(ert-deftest rackton-repl-buffer-fontifies-repl-commands ()
  "A comma command leading the prompt input highlights as a REPL command;
a comma elsewhere (an unquote) does not."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert "λ> ,type (sqr 2)")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward ",type")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'rackton-repl-command-face))
    ;; An unquote of a like-named variable, not at the input's head, is
    ;; not a command and stays unhighlighted.
    (insert "\nλ> `(a ,type)")
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "`(a ,")
    (should-not (eq (get-text-property (point) 'face)
                    'rackton-repl-command-face))))

(ert-deftest rackton-repl-output-is-not-fontified-as-code ()
  "Process output is prose, not Rackton code, so the language's
keywords must not fontify it; input on the same buffer still must."
  (with-temp-buffer
    (inferior-rackton-mode)
    ;; A ,info reply as the process prints it: comint tags output with
    ;; the `field' property `output'.
    (insert (propertize "Contravariant (class)\n    contramap :: (All t a b)"
                        'field 'output))
    ;; An input form the user typed (field nil, as comint leaves input).
    (insert "\n(define (eff x) (Some x))")
    (font-lock-ensure)
    ;; The banner prose must carry no code faces: the header name and
    ;; the (class) annotation are not Rackton code.
    (goto-char (point-min))
    (search-forward "Contravariant")
    (should-not (get-text-property (match-beginning 0) 'face))
    (search-forward "class")
    (should-not (get-text-property (match-beginning 0) 'face))
    ;; `All', after `::', sits in the reply's type region — but it is
    ;; the quantifier, so it reads as a keyword, not a type.
    (search-forward "All")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-keyword-face))
    ;; The input form is still highlighted as before.
    (search-forward "define")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-keyword-face))
    (search-forward "Some")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'rackton-constructor-face))))

(ert-deftest rackton-repl-fontifies-reply-types-after-double-colon ()
  "The type to the right of `::' in a ,type/,accepts/,search reply is a
type expression; the head name to its left is not."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert (propertize
             (concat "3 :: Integer\n"
                     "abs :: (All (a) ((Num a) => (-> a a)))\n")
             'field 'output))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Integer")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-type-face))
    (search-forward "Num")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-type-face))))

(ert-deftest rackton-repl-keeps-reply-constructor-head-unhighlighted ()
  "A ,info constructor line lists `Name :: scheme'; the capitalized type
in the scheme is a type, but the constructor head left of `::' is not."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert (propertize "    None :: (All (a) (Maybe a))\n" 'field 'output))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "None")
    (should-not (eq (get-text-property (match-beginning 0) 'face)
                    'font-lock-type-face))
    (search-forward "Maybe")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-type-face))))

(ert-deftest rackton-repl-fontifies-forall-in-reply-as-keyword ()
  "The quantifier heading a reply's scheme — `All' or `∀' — is a keyword,
not a type, even though it sits in the reply's type region."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert (propertize
             (concat "abs :: (All (a) (-> a a))\n"
                     "foo :: (∀ (a) (-> a a))\n")
             'field 'output))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "All")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-keyword-face))
    (search-forward "∀")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-keyword-face))))

(ert-deftest rackton-repl-fontifies-info-instance-heads ()
  "The bare type-level heads of a ,info reply — instance/implements and
superprotocol heads with no `::' — are type expressions."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert (propertize
             (concat "  superprotocols: (Applicative m)\n"
                     "  implements:\n"
                     "    (Monad Maybe)\n"
                     "    (Functor Maybe)\n")
             'field 'output))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Applicative")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-type-face))
    (search-forward "Monad")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-type-face))
    (search-forward "Functor")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-type-face))))

(ert-deftest rackton-repl-fontifies-wrapped-reply-type-continuation ()
  "A long scheme wraps onto hanging continuation lines; the type names on
those continuations are still types."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert (propertize
             (concat "    flatmap\n"
                     "       :: (All\n"
                     "         (m a b)\n"
                     "         ((Monad m) => (-> (-> a (m b)) (m a) (m b))))\n")
             'field 'output))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Monad")             ; on a continuation line
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-type-face))))

(ert-deftest rackton-repl-leaves-info-labels-and-laws-unhighlighted ()
  "A ,info law body is Rackton code, not a type list, so its head is not
read as a type; prose labels stay plain."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert (propertize
             (concat "  laws:\n"
                     "    left-identity:\n"
                     "      ((Eq (m Integer))\n")
             'field 'output))
    (font-lock-ensure)
    ;; The law body line begins `((Eq …' — `((' excludes it from the
    ;; instance-head rule, so `Eq' is not mass-highlighted as a type.
    (goto-char (point-min))
    (search-forward "Eq")
    (should-not (eq (get-text-property (match-beginning 0) 'face)
                    'font-lock-type-face))))

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

;;; Unit: type query fallback

(ert-deftest rackton-repl-single-identifier-p-recognizes-bare-names ()
  "A bare name is an identifier; compound or empty input is not."
  (should (rackton-repl--single-identifier-p "get-st"))
  (should (rackton-repl--single-identifier-p "  get-st  ")) ; trimmed
  (should-not (rackton-repl--single-identifier-p "(foo bar)"))
  (should-not (rackton-repl--single-identifier-p "foo bar"))
  (should-not (rackton-repl--single-identifier-p ""))
  (should-not (rackton-repl--single-identifier-p nil)))

(defun rackton-test--stub-query (replies)
  "A `rackton-repl-query' stub returning REPLIES keyed by command head.
REPLIES maps a meta-command string like \",type\" to its reply."
  (lambda (input &rest _)
    (let ((head (car (split-string input))))
      (or (cdr (assoc head replies))
          (error "Unexpected query: %s" input)))))

(ert-deftest rackton-repl-type-or-scheme-passes-through-a-type ()
  "When `,type' yields a type, it is returned and `,info' is not asked."
  (cl-letf (((symbol-function 'rackton-repl-query)
             (rackton-test--stub-query
              ;; ,info deliberately errors: reaching it would fail the test.
              '((",type" . "foo :: Integer")))))
    (should (equal (rackton-repl--type-or-scheme "foo") "foo :: Integer"))))

(ert-deftest rackton-repl-type-or-scheme-falls-back-for-identifiers ()
  "An identifier whose `,type' has no type falls back to the `,info' scheme."
  (cl-letf (((symbol-function 'rackton-repl-query)
             (rackton-test--stub-query
              '((",type" . "error: infer: ambiguous use of get-st")
                (",info" . "get-st :: (All (s m) ((MonadState s m) => (m s)))")))))
    (should (equal (rackton-repl--type-or-scheme "get-st")
                   "get-st :: (All (s m) ((MonadState s m) => (m s)))"))))

(ert-deftest rackton-repl-type-or-scheme-no-fallback-for-compound ()
  "A compound expression keeps the `,type' error; `,info' is not asked."
  (cl-letf (((symbol-function 'rackton-repl-query)
             (rackton-test--stub-query
              '((",type" . "error: infer: ambiguous")))))
    (should (string-prefix-p "error:"
                             (rackton-repl--type-or-scheme "(get-st x)")))))

(ert-deftest rackton-repl-eldoc-defers-to-eglot ()
  "With eglot managing the buffer, the REPL eldoc stays silent so the
LSP server's hover is the single source of type-at-point."
  (cl-letf (((symbol-function 'rackton-repl--live-p) (lambda () t))
            ((symbol-function 'eglot-managed-p) (lambda () t))
            ;; A type the REPL *would* report, so only the eglot guard
            ;; can keep the callback from firing.
            ((symbol-function 'rackton-repl--type-of) (lambda (_) "map :: whatever")))
    (with-temp-buffer
      (clrhash rackton-repl--type-cache)
      (insert "map")
      (goto-char (point-min))
      (let (called)
        (rackton-repl-eldoc (lambda (&rest _) (setq called t)))
        (should-not called)))))

(ert-deftest rackton-repl-eldoc-reports-when-eglot-absent ()
  "Without eglot, the REPL eldoc still reports the cached type."
  (cl-letf (((symbol-function 'rackton-repl--live-p) (lambda () t))
            ((symbol-function 'eglot-managed-p) (lambda () nil))
            ((symbol-function 'rackton-repl--type-of)
             (lambda (_) "map :: (All (a b) (-> (-> a b) (-> (List a) (List b))))")))
    (with-temp-buffer
      (clrhash rackton-repl--type-cache)
      (insert "map")
      (goto-char (point-min))
      (let (doc)
        (rackton-repl-eldoc (lambda (d &rest _) (setq doc d)))
        (should (string-prefix-p "map ::" doc))))))

;;; Error navigation

(ert-deftest rackton-repl-error-at-point-parses-location ()
  "On an error line, the location FILE LINE COL is recovered."
  (with-temp-buffer
    (insert "error: Racket/rackton-example.rkt:31:0: infer: wrong type\n")
    (goto-char (point-min))
    (should (equal (rackton-repl--error-at-point)
                   '("Racket/rackton-example.rkt" 31 0))))
  ;; A non-error line yields nil.
  (with-temp-buffer
    (insert "(require \"foo.rkt\")\n")
    (goto-char (point-min))
    (should-not (rackton-repl--error-at-point))))

(ert-deftest rackton-repl-visit-error-opens-source-at-location ()
  "Visiting a location lands at the 1-based line and 0-based column."
  (let ((file (make-temp-file "rackton-err" nil ".rkt")))
    (with-temp-file file (insert "line one\nline two\nABCDEF\n"))
    (unwind-protect
        (save-window-excursion
          (rackton-repl--visit-error (list file 3 2))
          (should (equal (buffer-file-name) file))
          (should (= (line-number-at-pos) 3))
          (should (= (current-column) 2)))
      (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest rackton-repl-visit-error-moves-point-when-already-shown ()
  "Point moves to the location even when the file is already displayed.
Regression: a buffer shown in a window keeps its own window-point, so
point must be set after the window is selected, not before."
  (let ((file (make-temp-file "rackton-err" nil ".rkt")))
    (with-temp-file file (insert "line one\nline two\nABCDEF\n"))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (let ((buf (find-file-noselect file))
                (other (split-window)))
            ;; Show the file in `other', point at the top, while a
            ;; different window stays selected.
            (set-window-buffer other buf)
            (set-window-point other (point-min))
            (with-current-buffer buf (goto-char (point-min)))
            (should-not (eq (selected-window) other))
            (rackton-repl--visit-error (list file 3 2))
            (let ((win (get-buffer-window buf)))
              (should win)
              (should (= (with-current-buffer buf
                           (line-number-at-pos (window-point win)))
                         3)))))
      (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
      (delete-file file))))

(ert-deftest rackton-repl-error-line-is-fontified ()
  "The first line of an error gets the error face; output, so unwrapped."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert "error: foo.rkt:31:0: infer: wrong type")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'rackton-repl-error-face))))

(ert-deftest rackton-repl-return-visits-error-on-error-line ()
  "RET on an error line jumps to the source instead of submitting."
  (let (visited)
    (cl-letf (((symbol-function 'rackton-repl--visit-error)
               (lambda (loc) (setq visited loc))))
      (with-temp-buffer
        (inferior-rackton-mode)
        (insert "error: foo.rkt:31:0: infer: wrong type")
        (goto-char (point-min))
        (rackton-repl-return)
        (should (equal visited '("foo.rkt" 31 0)))))))

(ert-deftest rackton-repl-error-line-is-clickable ()
  "The error line carries a mouse-face and the error keymap."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert "error: foo.rkt:31:0: infer: wrong type")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'mouse-face) 'highlight))
    (should (eq (get-text-property (point) 'keymap) rackton-repl-error-map))))

(ert-deftest rackton-repl-mouse-visits-error ()
  "A click on an error line jumps to the source location."
  (let (visited)
    (cl-letf (((symbol-function 'rackton-repl--visit-error)
               (lambda (loc) (setq visited loc))))
      (with-temp-buffer
        (inferior-rackton-mode)
        (insert "error: foo.rkt:31:0: infer: wrong type")
        (set-window-buffer (selected-window) (current-buffer))
        (let ((event (list 'mouse-2 (list (selected-window) (point-min)))))
          (rackton-repl-visit-error-at-mouse event)
          (should (equal visited '("foo.rkt" 31 0))))))))

(ert-deftest rackton-repl-error-detail-line-p-recognizes-block ()
  "Indented lines below an `error:' line are detail; the head is not."
  (with-temp-buffer
    (insert "error: foo.rkt:1:0: bad\n  expected: (Kleisli a)\n  in: (x)\n")
    (goto-char (point-min))
    (should-not (rackton-repl--error-detail-line-p (point))) ; the error: line
    (forward-line 1)
    (should (rackton-repl--error-detail-line-p (point)))     ; expected:
    (forward-line 1)
    (should (rackton-repl--error-detail-line-p (point))))    ; in:
  ;; An indented line not under an error (a ,info reply) is not detail.
  (with-temp-buffer
    (insert "Contravariant (class)\n  methods:\n")
    (goto-char (point-min))
    (forward-line 1)
    (should-not (rackton-repl--error-detail-line-p (point)))))

(ert-deftest rackton-repl-error-detail-context-splits-type-and-code ()
  "expected:/got: lines are type context; in: and below are code."
  (with-temp-buffer
    (insert (concat "error: f.rkt:1:0: bad\n"
                    "  expected: (Maybe a)\n"
                    "  got:      (List Integer)\n"
                    "  in: (define x (Just 1))\n"
                    "        (more)\n"))
    (goto-char (point-min))
    (should-not (rackton-repl--error-detail-context (point))) ; head
    (forward-line 1)
    (should (eq (rackton-repl--error-detail-context (point)) 'type))   ; expected
    (forward-line 1)
    (should (eq (rackton-repl--error-detail-context (point)) 'type))   ; got
    (forward-line 1)
    (should (eq (rackton-repl--error-detail-context (point)) 'code))   ; in:
    (forward-line 1)
    (should (eq (rackton-repl--error-detail-context (point)) 'code)))) ; in: continuation

(ert-deftest rackton-repl-type-detail-reads-applications-as-types ()
  "In expected/got, a type application like (Maybe a) is a type, not a ctor;
after in:, a constructor application stays a constructor."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert (propertize
             (concat "error: foo.rkt:1:0: infer: bad\n"
                     "  expected: (Maybe a)\n"
                     "  got:      (List Integer)\n"
                     "  in: (define x (Just 1))\n")
             'field 'output))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Maybe")
    (should (eq (get-text-property (match-beginning 0) 'face) 'font-lock-type-face))
    (search-forward "List")
    (should (eq (get-text-property (match-beginning 0) 'face) 'font-lock-type-face))
    (search-forward "Integer")
    (should (eq (get-text-property (match-beginning 0) 'face) 'font-lock-type-face))
    (search-forward "Just")
    (should (eq (get-text-property (match-beginning 0) 'face) 'rackton-constructor-face))))

(ert-deftest rackton-repl-error-detail-is-fontified-as-code ()
  "The types and form after the first error line get Rackton faces."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert (propertize
             (concat "error: foo.rkt:1:0: infer: bad\n"
                     "  expected: (-> (Kleisli a) Boolean)\n"
                     "  in: (instance (Category (Kleisli m))"
                     " (define (comp x) (Kleisli x)))\n")
             'field 'output))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "Kleisli")            ; type position in `expected:'
    (should (eq (get-text-property (match-beginning 0) 'face) 'font-lock-type-face))
    (search-forward "instance")
    (should (eq (get-text-property (match-beginning 0) 'face) 'font-lock-keyword-face))
    (search-forward "Category")
    (should (eq (get-text-property (match-beginning 0) 'face) 'font-lock-type-face))
    (goto-char (point-min))
    (search-forward "(Kleisli x)")        ; constructor position in `in:'
    (goto-char (match-beginning 0))
    (search-forward "Kleisli")
    (should (eq (get-text-property (match-beginning 0) 'face) 'rackton-constructor-face))))

(ert-deftest rackton-repl-error-labels-are-fontified ()
  "The expected:/got:/in: labels get the error-label face."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert (propertize "error: foo.rkt:1:0: bad\n  expected: (Maybe a)\n"
                        'field 'output))
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "expected:")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'rackton-repl-error-label-face))))

(ert-deftest rackton-repl-noninerror-output-prose-stays-plain ()
  "Indented non-error output (a ,info reply) keeps its prose plain: the
header name and method head are not code.  The type after `::', though,
is now a type (see the reply-type tests)."
  (with-temp-buffer
    (inferior-rackton-mode)
    (insert (propertize
             (concat "Contravariant (class)\n  methods:\n"
                     "    contramap :: (-> (Predicate a))\n")
             'field 'output))
    (font-lock-ensure)
    ;; The header name and the lowercase method head are prose.
    (goto-char (point-min))
    (search-forward "Contravariant")
    (should-not (get-text-property (match-beginning 0) 'face))
    (search-forward "contramap")
    (should-not (get-text-property (match-beginning 0) 'face))
    ;; The type right of `::' is a type.
    (search-forward "Predicate")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'font-lock-type-face))))

;;; History navigation

(ert-deftest rackton-repl-mp-bound-to-history-search ()
  "M-p runs the position-sensitive history command in the REPL."
  (should (eq (lookup-key inferior-rackton-mode-map (kbd "M-p"))
              'rackton-repl-previous-input)))

(ert-deftest rackton-repl-mn-bound-to-history-search ()
  "M-n runs the position-sensitive forward-history command in the REPL."
  (should (eq (lookup-key inferior-rackton-mode-map (kbd "M-n"))
              'rackton-repl-next-input)))

(defun rackton-test--wait-for-prompt ()
  "Wait for the REPL buffer to end at a fresh prompt."
  (rackton-test--wait-for
   (lambda ()
     (with-current-buffer (rackton-repl--buffer)
       (save-excursion
         (goto-char (point-max))
         (forward-line 0)
         (looking-at-p rackton-repl-prompt-regexp))))
   15))

(ert-deftest rackton-repl-previous-input-matches-text-before-point ()
  "With text before point, M-p recalls the previous input starting with it."
  (rackton-test--ensure-repl)
  (rackton-repl--send-form "(define alpha-xyz 1)")
  (rackton-test--wait-for-prompt)
  (rackton-repl--send-form "(define beta-xyz 2)")
  (rackton-test--wait-for-prompt)
  (with-current-buffer (rackton-repl--buffer)
    (let ((proc (get-buffer-process (current-buffer))))
      (goto-char (point-max))
      (delete-region (process-mark proc) (point-max))
      (unwind-protect
          (progn
            (goto-char (point-max))
            (insert "(define al")       ; point after "al" — not the input start
            (setq last-command nil comint-input-ring-index nil)
            (rackton-repl-previous-input 1)
            (let ((input (buffer-substring-no-properties
                          (process-mark proc) (point-max))))
              (should (string-match-p "alpha-xyz" input))
              (should-not (string-match-p "beta-xyz" input))))
        (delete-region (process-mark proc) (point-max))))))

(ert-deftest rackton-repl-previous-input-at-start-cycles-all ()
  "At the input start, M-p recalls the most recent input regardless of text."
  (rackton-test--ensure-repl)
  (rackton-repl--send-form "(define gamma-xyz 9)")
  (rackton-test--wait-for-prompt)
  (with-current-buffer (rackton-repl--buffer)
    (let ((proc (get-buffer-process (current-buffer))))
      (goto-char (point-max))
      (delete-region (process-mark proc) (point-max))
      (unwind-protect
          (progn
            (goto-char (process-mark proc))  ; at the input start
            (setq last-command nil comint-input-ring-index nil)
            (rackton-repl-previous-input 1)
            (should (string-match-p
                     "gamma-xyz"
                     (buffer-substring-no-properties
                      (process-mark proc) (point-max)))))
        (delete-region (process-mark proc) (point-max))))))

(ert-deftest rackton-repl-next-input-mirrors-previous-when-matching ()
  "In a matching run, M-n moves forward through the matched set."
  (rackton-test--ensure-repl)
  (rackton-repl--send-form "(define nnn-a 1)")
  (rackton-test--wait-for-prompt)
  (rackton-repl--send-form "(define nnn-b 2)")
  (rackton-test--wait-for-prompt)
  (rackton-repl--send-form "(define nnn-c 3)")
  (rackton-test--wait-for-prompt)
  (with-current-buffer (rackton-repl--buffer)
    (let ((proc (get-buffer-process (current-buffer))))
      (goto-char (point-max))
      (delete-region (process-mark proc) (point-max))
      (unwind-protect
          (progn
            (goto-char (point-max))
            (insert "(define nnn-")
            (setq last-command nil comint-input-ring-index nil)
            (rackton-repl-previous-input 1)   ; -> nnn-c
            (setq last-command 'rackton-repl-previous-input)
            (rackton-repl-previous-input 1)   ; -> nnn-b
            (setq last-command 'rackton-repl-previous-input)
            (rackton-repl-next-input 1)       ; -> nnn-c (forward again)
            (should (string-match-p
                     "nnn-c"
                     (buffer-substring-no-properties
                      (process-mark proc) (point-max)))))
        (delete-region (process-mark proc) (point-max))))))

;;; Completion

(ert-deftest rackton-repl-completions-parses-reply ()
  "The completion helper splits the ,complete reply into candidates."
  (cl-letf (((symbol-function 'rackton-repl-query)
             (lambda (&rest _) "match\nmax\n")))
    (should (equal (rackton-repl--completions "ma") '("match" "max"))))
  (cl-letf (((symbol-function 'rackton-repl-query) (lambda (&rest _) "")))
    (should-not (rackton-repl--completions "zz"))))

(ert-deftest rackton-repl-completion-at-point-returns-candidates ()
  "The capf returns the symbol bounds and the REPL's candidates."
  (cl-letf (((symbol-function 'rackton-repl--live-p) (lambda () t))
            ((symbol-function 'eglot-managed-p) (lambda () nil))
            ((symbol-function 'rackton-repl--completions)
             (lambda (_) '("match" "max"))))
    (with-temp-buffer
      (insert "ma")
      (let ((res (rackton-repl-completion-at-point)))
        (should (= (nth 0 res) (point-min)))
        (should (= (nth 1 res) (point-max)))
        (should (equal (nth 2 res) '("match" "max")))
        (should (eq (plist-get (nthcdr 3 res) :exclusive) 'no))))))

(ert-deftest rackton-repl-completion-defers-to-eglot ()
  "The capf yields nothing when eglot manages the buffer."
  (cl-letf (((symbol-function 'rackton-repl--live-p) (lambda () t))
            ((symbol-function 'eglot-managed-p) (lambda () t))
            ((symbol-function 'rackton-repl--completions) (lambda (_) '("match"))))
    (with-temp-buffer
      (insert "ma")
      (should-not (rackton-repl-completion-at-point)))))

(ert-deftest rackton-repl-completion-quiet-without-repl ()
  "The capf yields nothing when no REPL is running."
  (cl-letf (((symbol-function 'rackton-repl--live-p) (lambda () nil)))
    (with-temp-buffer
      (insert "ma")
      (should-not (rackton-repl-completion-at-point)))))

(ert-deftest rackton-repl-mode-installs-completion ()
  "The REPL buffer wires the capf and makes TAB complete."
  (with-temp-buffer
    (inferior-rackton-mode)
    (should (memq 'rackton-repl-completion-at-point completion-at-point-functions))
    (should (eq tab-always-indent 'complete))))

(ert-deftest rackton-mode-installs-source-completion ()
  "A source buffer wires the REPL-backed capf and makes TAB complete."
  (with-temp-buffer
    (rackton-mode)
    (should (eq tab-always-indent 'complete))
    (should (memq 'rackton-repl-completion-at-point completion-at-point-functions))))

(ert-deftest rackton-repl-complete-finds-prelude-name ()
  "Integration: ,complete over the pipe returns a known prelude name."
  (rackton-test--ensure-repl)
  (should (member "match" (rackton-repl--completions "ma"))))

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

(ert-deftest rackton-repl-return-inside-complete-sexp-opens-line ()
  ;; A balanced form, but point sits inside it: enter must open a line,
  ;; not submit.
  (rackton-test--ensure-repl)
  (with-current-buffer (rackton-repl--buffer)
    (goto-char (point-max))
    (let ((input-start (process-mark (rackton-repl--process))))
      (unwind-protect
          (progn
            (insert "(+ 1 2)")
            (backward-char 1)           ; point inside the form, before ")"
            (rackton-repl-return)
            (let ((region (buffer-substring-no-properties
                           input-start (point-max))))
              ;; Not submitted: the form is still being edited...
              (should (string-prefix-p "(+ 1 2" region))
              ;; ...with a freshly opened line inside it.
              (should (string-match-p "\n" region))))
        (delete-region input-start (point-max))))))

(ert-deftest rackton-repl-multiline-send-shows-no-continuation-prompts ()
  (rackton-test--ensure-repl)
  (rackton-repl--send-form "(define (tenth x)\n  (* 10 x))")
  (should (rackton-test--wait-for
           (lambda () (rackton-test--repl-contains-p "tenth ::"))
           15))
  (should-not (rackton-test--repl-contains-p "..>")))

(ert-deftest rackton-repl-prompt-is-preceded-by-blank-line ()
  (rackton-test--ensure-repl)
  (rackton-repl--send-form "(+ 20 3)")
  (should (rackton-test--wait-for
           (lambda () (rackton-test--repl-contains-p "23 :: Integer"))
           15))
  (with-current-buffer (rackton-repl--buffer)
    (save-excursion
      (goto-char (point-max))
      (forward-line 0)                  ; start of the latest prompt line
      (should (looking-at rackton-repl-prompt-regexp))
      (forward-line -1)
      (should (looking-at "^[ \t]*$")))))

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

(ert-deftest rackton-repl-search-sends-search-command ()
  "rackton-repl-search issues ,search and shows the reply."
  (let (sent)
    (cl-letf (((symbol-function 'rackton-repl-query)
               (lambda (input &rest _) (setq sent input) "length :: …"))
              ((symbol-function 'display-buffer) #'ignore))
      (rackton-repl-search "(-> (List a) Integer)")
      (should (equal sent ",search (-> (List a) Integer)"))
      (with-current-buffer "*rackton-doc*"
        (should (string-prefix-p "length" (buffer-string)))))))

(ert-deftest rackton-repl-returns-sends-returns-command ()
  "rackton-repl-returns issues ,returns and shows the reply."
  (let (sent)
    (cl-letf (((symbol-function 'rackton-repl-query)
               (lambda (input &rest _) (setq sent input) "filter :: …"))
              ((symbol-function 'display-buffer) #'ignore))
      (rackton-repl-returns "(List Integer)")
      (should (equal sent ",returns (List Integer)"))
      (with-current-buffer "*rackton-doc*"
        (should (string-prefix-p "filter" (buffer-string)))))))

(ert-deftest rackton-repl-binds-session-search-keys ()
  "The session search commands are on the C-c C-f prefix, Control variants."
  (should (eq (lookup-key rackton-mode-map (kbd "C-c C-f C-s")) 'rackton-repl-search))
  (should (eq (lookup-key rackton-mode-map (kbd "C-c C-f C-r")) 'rackton-repl-returns)))

(ert-deftest rackton-repl-search-fills-doc-buffer ()
  "Integration: ,search over the session finds a known signature match."
  (rackton-test--ensure-repl)
  (cl-letf (((symbol-function 'display-buffer) #'ignore))
    (rackton-repl-search "(-> (List a) Integer)"))
  (with-current-buffer "*rackton-doc*"
    (save-excursion
      (goto-char (point-min))
      (should (search-forward "length" nil t)))))

(ert-deftest rackton-repl-returns-fills-doc-buffer ()
  "Integration: ,returns over the session finds a function returning the type."
  (rackton-test--ensure-repl)
  (cl-letf (((symbol-function 'display-buffer) #'ignore))
    (rackton-repl-returns "(List Integer)"))
  (with-current-buffer "*rackton-doc*"
    (save-excursion
      (goto-char (point-min))
      (should (search-forward "filter" nil t)))))

(ert-deftest rackton-repl-clear-buffer-erases-output-keeps-session ()
  (rackton-test--ensure-repl)
  ;; A definition and some output to clear away.
  (rackton-repl-query "(define (clr-probe x) x)")
  (rackton-repl--send-form "(* 3 4)")
  (should (rackton-test--wait-for
           (lambda () (rackton-test--repl-contains-p "12 :: Integer"))
           15))
  (rackton-repl-clear-buffer)
  ;; The output is gone...
  (should-not (rackton-test--repl-contains-p "12 :: Integer"))
  ;; ...but the process is alive and the buffer still ends at a prompt...
  (should (process-live-p (rackton-repl--process)))
  (with-current-buffer (rackton-repl--buffer)
    (save-excursion
      (goto-char (point-max))
      (forward-line 0)
      (should (looking-at rackton-repl-prompt-regexp))))
  ;; ...and the session survived: the earlier definition is still bound.
  (should (string-match-p "::" (rackton-repl-query ",type clr-probe"))))

(ert-deftest rackton-repl-clear-buffer-works-from-other-buffer ()
  (rackton-test--ensure-repl)
  (rackton-repl--send-form "(* 9 9)")
  (should (rackton-test--wait-for
           (lambda () (rackton-test--repl-contains-p "81 :: Integer"))
           15))
  (with-temp-buffer                     ; not the REPL buffer
    (rackton-mode)
    (rackton-repl-clear-buffer))
  (should-not (rackton-test--repl-contains-p "81 :: Integer")))

(ert-deftest rackton-repl-clear-buffer-errors-without-repl ()
  (cl-letf (((symbol-function 'rackton-repl--buffer) (lambda () nil)))
    (should-error (rackton-repl-clear-buffer) :type 'user-error)))

;;; Integration: session reset

(ert-deftest rackton-repl-reset-wipes-session ()
  (rackton-test--ensure-repl)
  (rackton-repl-query "(define (reset-probe x) x)")
  (should (string-match-p "::" (rackton-repl-query ",type reset-probe")))
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
    (rackton-repl-reset))
  ;; Wait for ,clear to land (its visible confirmation), then the
  ;; earlier definition is gone.
  (should (rackton-test--wait-for
           (lambda () (rackton-test--repl-contains-p "session cleared"))
           15))
  (should-not (string-match-p "::" (rackton-repl-query ",type reset-probe"))))

(ert-deftest rackton-repl-reset-declined-keeps-session ()
  (rackton-test--ensure-repl)
  (rackton-repl-query "(define (keep-probe x) x)")
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
    (rackton-repl-reset))
  ;; Declining sends nothing: the binding survives.
  (should (string-match-p "::" (rackton-repl-query ",type keep-probe"))))

(ert-deftest rackton-repl-reset-clears-type-cache ()
  (rackton-test--ensure-repl)
  (puthash "stale" "stale :: Integer" rackton-repl--type-cache)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
    (rackton-repl-reset))
  (should (= 0 (hash-table-count rackton-repl--type-cache))))

(ert-deftest rackton-repl-reset-errors-without-repl ()
  (cl-letf (((symbol-function 'rackton-repl--live-p) (lambda () nil)))
    (should-error (rackton-repl-reset) :type 'user-error)))

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

;;; menu

(ert-deftest rackton-repl-menu-offers-eval-commands ()
  "The Rackton menu offers the evaluation commands."
  (let ((cmds (rackton-test--menu-commands (rackton-test--rackton-menu))))
    (dolist (c '(rackton-eval-last-sexp rackton-eval-defun
                 rackton-send-region rackton-eval-buffer))
      (should (memq c cmds)))))

(ert-deftest rackton-repl-menu-offers-inspect-commands ()
  "The Rackton menu offers the type/describe/source/accepts commands."
  (let ((cmds (rackton-test--menu-commands (rackton-test--rackton-menu))))
    (dolist (c '(rackton-type rackton-describe-symbol
                 rackton-show-source rackton-accepts))
      (should (memq c cmds)))))

(ert-deftest rackton-repl-menu-offers-repl-commands ()
  "The Rackton menu offers REPL control, including session search."
  (let ((cmds (rackton-test--menu-commands (rackton-test--rackton-menu))))
    (dolist (c '(rackton-repl rackton-repl-clear-buffer rackton-repl-reset
                 rackton-repl-search rackton-repl-returns))
      (should (memq c cmds)))))

;;; Type annotations

(ert-deftest rackton-repl-type-from-reply-extracts-type ()
  "The type is everything right of `::', with whitespace collapsed."
  (should (equal (rackton-repl--type-from-reply "foo :: Integer") "Integer"))
  (should (equal (rackton-repl--type-from-reply
                  "foo :: (All\n          (a)\n          (-> a a))")
                 "(All (a) (-> a a))"))
  (should-not (rackton-repl--type-from-reply "error: unbound foo")))

(defmacro rackton-test--with-annotation (code reply &rest body)
  "Insert CODE in a `rackton-mode' buffer with REPLY stubbed for `,type'.
Point starts at `point-min'; eval BODY with `rackton-repl-query' stubbed."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'rackton-repl-query)
              (rackton-test--stub-query (list (cons ",type" ,reply)))))
     (with-temp-buffer
       (insert ,code)
       (rackton-mode)
       (goto-char (point-min))
       ,@body)))

(ert-deftest rackton-annotate-inserts-when-absent ()
  "With no signature above, one is inserted matching the inferred type."
  (rackton-test--with-annotation
      "(define (sqr x) (* x x))\n" "sqr :: (-> Integer Integer)"
    (search-forward "(define (")        ; point on the bound name `sqr'
    (rackton-annotate-definition)
    (should (equal (buffer-string)
                   "(: sqr (-> Integer Integer))\n(define (sqr x) (* x x))\n"))))

(ert-deftest rackton-annotate-fixes-wrong-type ()
  "An existing signature whose type disagrees is rewritten."
  (rackton-test--with-annotation
      "(: sqr (-> String String))\n(define (sqr x) (* x x))\n"
      "sqr :: (-> Integer Integer)"
    (search-forward "(define (")
    (rackton-annotate-definition)
    (should (equal (buffer-string)
                   "(: sqr (-> Integer Integer))\n(define (sqr x) (* x x))\n"))))

(ert-deftest rackton-annotate-leaves-correct-alone ()
  "A signature already agreeing with the inferred type is untouched."
  (rackton-test--with-annotation
      "(: sqr (-> Integer Integer))\n(define (sqr x) (* x x))\n"
      "sqr :: (-> Integer Integer)"
    (search-forward "(define (")
    (let ((before (buffer-string)))
      (rackton-annotate-definition)
      (should (equal (buffer-string) before)))))

(ert-deftest rackton-annotate-ignores-whitespace-differences ()
  "A signature differing only in whitespace counts as correct, so untouched."
  (rackton-test--with-annotation
      "(: sqr (->  Integer\n            Integer))\n(define (sqr x) (* x x))\n"
      "sqr :: (-> Integer Integer)"
    (search-forward "(define (")
    (let ((before (buffer-string)))
      (rackton-annotate-definition)
      (should (equal (buffer-string) before)))))

(ert-deftest rackton-annotate-handles-value-define ()
  "A (define name value) form, not just a function, gets a signature."
  (rackton-test--with-annotation
      "(define answer 42)\n" "answer :: Integer"
    (search-forward "(define ")          ; point on `answer'
    (rackton-annotate-definition)
    (should (equal (buffer-string)
                   "(: answer Integer)\n(define answer 42)\n"))))

(ert-deftest rackton-annotate-preserves-indentation ()
  "The inserted signature is indented to match the define it heads."
  (rackton-test--with-annotation
      "  (define (sqr x) (* x x))\n" "sqr :: (-> Integer Integer)"
    (search-forward "(define (")
    (rackton-annotate-definition)
    (should (equal (buffer-string)
                   "  (: sqr (-> Integer Integer))\n  (define (sqr x) (* x x))\n"))))

(ert-deftest rackton-annotate-requires-point-on-name ()
  "Point off the bound name (in the body) is a `user-error', no edit."
  (with-temp-buffer
    (insert "(define (sqr x) (* x x))\n")
    (rackton-mode)
    (goto-char (point-min))
    (search-forward "* x")               ; in the body, not on `sqr'
    (should-error (rackton-annotate-definition) :type 'user-error)))

(ert-deftest rackton-annotate-errors-without-type ()
  "When the REPL reports no type for the name, the command errors."
  (cl-letf (((symbol-function 'rackton-repl-query)
             (rackton-test--stub-query '((",type" . "error: unbound sqr")
                                         (",info" . "error: unbound sqr")))))
    (with-temp-buffer
      (insert "(define (sqr x) (* x x))\n")
      (rackton-mode)
      (goto-char (point-min))
      (search-forward "(define (")
      (should-error (rackton-annotate-definition) :type 'user-error))))

(provide 'rackton-repl-test)
;;; rackton-repl-test.el ends here
