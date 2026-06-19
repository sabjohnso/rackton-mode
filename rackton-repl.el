;;; rackton-repl.el --- Inferior REPL for the Rackton language  -*- lexical-binding: t; -*-

;; Author: Samuel B. Johnson <samuel.bryant.johnson@gmail.com>
;; Version: 0.4.33
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages, processes

;;; Commentary:

;; A SLIME-flavored development environment for Rackton, built on the
;; stock `racket -l rackton/repl' process.  Three layers, each only
;; talking to the one below:
;;
;;   1. Transport — `inferior-rackton-mode', a comint mode owning the
;;      REPL process (`rackton-repl' starts or focuses it).
;;   2. Query channel — `rackton-repl-query' performs one
;;      command/response exchange over `comint-redirect' without
;;      touching the visible REPL buffer.
;;   3. UI commands — evaluation (`rackton-eval-defun' and friends),
;;      `rackton-type', `rackton-describe-symbol',
;;      `rackton-show-source', `rackton-accepts', and eldoc.
;;
;; The layering is the upgrade path: Rackton's LSP server, debug
;; server, and hoogle-style search service are under development
;; upstream.  When they ship, each UI command can move to the richer
;; backend by swapping its layer-2 call — none of the keybindings,
;; buffers, or command names need to change.

;;; Code:

(require 'comint)
(require 'subr-x)
(require 'thingatpt)
(require 'rackton-mode)

;;; Customization
;;
;; `rackton-program' (the Racket binary) lives in `rackton-mode', the
;; shared base every tool launches through; only the REPL's own
;; arguments are stated here.

(defcustom rackton-repl-arguments '("-l" "rackton/repl")
  "Arguments passed to `rackton-program' to boot the REPL."
  :type '(repeat string)
  :group 'rackton)

(defconst rackton-repl-prompt-regexp "^λ> *"
  "Regexp matching the Rackton REPL prompt.")

(defconst rackton-repl--buffer-name "*rackton-repl*")

(defconst rackton-repl-commands
  '("type" "info" "source" "accepts" "search" "returns" "clear" "complete")
  "REPL meta-commands, named without their leading comma.
The single statement of which `,'-prefixed directives the REPL knows;
the UI commands send these and font-lock highlights them.")

(defface rackton-repl-error-face
  '((t :inherit error))
  "Face for the first line of a Rackton REPL error."
  :group 'rackton)

(defface rackton-repl-error-label-face
  '((t :inherit bold))
  "Face for the expected:/got:/in: labels in a Rackton REPL error."
  :group 'rackton)

(defface rackton-repl-command-face
  '((t :inherit font-lock-keyword-face))
  "Face for a `,'-prefixed REPL meta-command at the prompt.
Inherits the keyword face by default, but is separate so a REPL
directive can be themed apart from the language's own keywords."
  :group 'rackton)

(defconst rackton-repl--error-line-regexp
  "^error: \\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\):"
  "Match a Rackton error's leading FILE:LINE:COL on its first line.")

(defvar rackton-repl-error-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] #'rackton-repl-visit-error-at-mouse)
    (define-key map [mouse-1] #'rackton-repl-visit-error-at-mouse)
    map)
  "Keymap on a REPL error line: a click visits its source location.")

(defconst rackton-repl--error-font-lock-keywords
  '(("^error:.*$" 0
     (list 'face 'rackton-repl-error-face
           'mouse-face 'highlight
           'keymap rackton-repl-error-map
           'help-echo "mouse-1, mouse-2, or RET: visit this error location")
     t)
    ("^[ \t]+\\(expected:\\|got:\\|in:\\)" 1 'rackton-repl-error-label-face))
  "Font-lock for a REPL error's first line and its detail labels.
The first line gets the error face plus a `mouse-face' and the
`rackton-repl-error-map' keymap (so it is clickable); the
expected:/got:/in: labels get the label face.  Errors are process
output, so — unlike the language's `rackton-font-lock-keywords' —
these are installed unwrapped (not filtered by
`rackton-repl--code-only'); the detail *code* between the labels is
fontified by the language keywords, which `rackton-repl--code-at-p'
lets through on error-detail lines.")

(defconst rackton-repl--command-font-lock-keywords
  `((,(concat rackton-repl-prompt-regexp
              "\\(,\\(?:" (regexp-opt rackton-repl-commands) "\\)\\)\\_>")
     1 'rackton-repl-command-face))
  "Font-lock for a REPL meta-command leading the prompt input.
Anchored on the prompt so only the input's first token counts as a
command; a `,' elsewhere on the line is an unquote, not a directive.")

;;; Layer 1: transport

(defun rackton-repl--error-detail-context (pos)
  "Classify the error-detail line at POS as `type', `code', or nil.
Climbs the indented lines above POS to the block's non-indented head.
The head must be an `error:' line — so a ,info reply's indented lines,
whose head is not an error, return nil.  Within the block the
`expected:'/`got:' lines (and their wrapped continuations) are `type'
— pure type information — while the `in:' label and everything below
it are `code', a Rackton form.  Plain prose output returns nil."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (and (looking-at-p "[ \t]")
         (let ((context 'type))
           (catch 'result
             (while t
               (when (looking-at-p "[ \t]*in:")
                 (setq context 'code))
               (when (looking-at-p "error:")
                 (throw 'result context))
               (when (or (bobp) (not (looking-at-p "[ \t]")))
                 (throw 'result nil)) ; reached the top or left the block
               (forward-line -1)))))))

(defun rackton-repl--error-detail-line-p (pos)
  "Non-nil when POS lies on any detail line of an error block."
  (and (rackton-repl--error-detail-context pos) t))

(defun rackton-repl--code-at-p (pos)
  "Non-nil where POS holds Rackton code the language keywords should fontify.
True for user input (anything that is not process output) and for an
error's `in:' form, but not for the `expected:'/`got:' type detail (a
type, fontified separately) nor for other output — banners and
,info/,type replies, which are prose."
  (or (not (eq (get-text-property pos 'field) 'output))
      (eq (rackton-repl--error-detail-context pos) 'code)))

(defun rackton-repl--type-detail-at-p (pos)
  "Non-nil when POS is on an error's `expected:'/`got:' type detail."
  (eq (rackton-repl--error-detail-context pos) 'type))

;; The ,type/,accepts/,search/,info replies print `<head> :: <type>'
;; lines, their wrapped continuations, and — for ,info — bare type-level
;; heads like `(Monad Maybe)'.  Everything right of `::', its hanging
;; continuations, and those instance/superprotocol heads is pure type
;; information, so every capitalized name there names a type, the same
;; reading the error type detail gets.  The head left of `::', prose
;; labels, and law bodies are not.

(defun rackton-repl--line-has-type-colon-p ()
  "Non-nil when the current line carries a `::' type separator."
  (save-excursion
    (forward-line 0)
    (re-search-forward "::" (line-end-position) t)))

(defun rackton-repl--after-type-colon-p (pos)
  "Non-nil when a `::' precedes POS on POS's own line."
  (save-excursion
    (goto-char pos)
    (re-search-backward "::" (line-beginning-position) t)))

(defun rackton-repl--type-head-line-p (pos)
  "Non-nil when POS's line is a ,info bare type-level head.
A standalone instance/implements head `(Monad Maybe)' or a
`superprotocols: …' field; capitalized names on it are types.  A law
body begins `((' or `(=', which this rule excludes."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (or (looking-at-p "[ \t]+([A-Z]")
        (looking-at-p "[ \t]*superprotocols:"))))

(defun rackton-repl--wrapped-type-line-p (pos)
  "Non-nil when POS's line hangs as a continuation of a wrapped `:: type'.
A continuation carries no `::' and is indented more deeply than the
`::' line it hangs under; climbing stops at that shallower line."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((indent (current-indentation)))
      (and (not (rackton-repl--line-has-type-colon-p))
           (catch 'result
             (while (not (bobp))
               (forward-line -1)
               (let ((this (current-indentation)))
                 (cond
                  ((looking-at-p "[ \t]*$") (throw 'result nil)) ; blank: block ends
                  ((< this indent)          ; shallower line: a possible head
                   (throw 'result (and (rackton-repl--line-has-type-colon-p) t)))
                  ((rackton-repl--line-has-type-colon-p)
                   (throw 'result nil))     ; sibling `name ::', not our head
                  (t nil))))                ; another continuation; keep climbing
             nil)))))

(defun rackton-repl--reply-type-at-p (pos)
  "Non-nil when POS lies in the type region of a REPL command reply.
That is the text right of `::', its wrapped continuations, and ,info's
bare type-level heads.  Error detail keeps its own path
\(`rackton-repl--error-detail-context'), so it is excluded here."
  ;; The helpers below search, which would clobber the match data the
  ;; calling font-lock matcher set; preserve it so the type name, not a
  ;; `::' the predicate found, is what gets fontified.
  (save-match-data
    (and (eq (get-text-property pos 'field) 'output)
         (not (rackton-repl--error-detail-line-p pos))
         (or (rackton-repl--after-type-colon-p pos)
             (rackton-repl--type-head-line-p pos)
             (rackton-repl--wrapped-type-line-p pos)))))

;; A ,info reply on a sum type lists its data constructors as
;; `<indent>Name :: <scheme>' lines under a `constructors:' label.  The
;; head left of `::' there names a data constructor — unlike the
;; identical method lines a protocol's `methods:' label carries, which
;; name functions.  So the head gets the constructor face only when its
;; enclosing label is `constructors:'.

(defun rackton-repl--constructor-line-p (pos)
  "Non-nil when POS's line lists a constructor under a `constructors:' label.
Climbs to the nearest shallower line — the listing's label — and holds
only when that label is `constructors:'.  A blank line ends the block."
  (save-excursion
    (goto-char pos)
    (forward-line 0)
    (let ((indent (current-indentation)))
      (and (looking-at-p "[ \t]")
           (catch 'result
             (while (not (bobp))
               (forward-line -1)
               (cond
                ((looking-at-p "[ \t]*$") (throw 'result nil)) ; blank: block ends
                ((< (current-indentation) indent)
                 (throw 'result (and (looking-at-p "[ \t]*constructors:") t)))))
             nil)))))

(defun rackton-repl--constructor-head-at-p (pos)
  "Non-nil when POS is the head name of a `constructors:' listing line.
That is the capitalized name left of `::' — the data constructor — on a
line the `constructors:' label encloses."
  (save-match-data
    (and (eq (get-text-property pos 'field) 'output)
         (not (rackton-repl--after-type-colon-p pos))
         (rackton-repl--constructor-line-p pos))))

(defun rackton-repl--matcher-where (matcher pred)
  "Wrap font-lock MATCHER to fire only where PRED holds at the match start.
MATCHER is a regexp string or a matcher function; PRED takes a buffer
position.  Used to confine the language keywords to code, and the
type-name rule to error type detail."
  (lambda (limit)
    (let (hit)
      (while (and (setq hit (if (functionp matcher)
                                (funcall matcher limit)
                              (re-search-forward matcher limit t)))
                  (not (funcall pred (match-beginning 0)))))
      hit)))

(define-derived-mode inferior-rackton-mode comint-mode "Inferior Rackton"
  "Major mode for the inferior Rackton REPL.

\\{inferior-rackton-mode-map}"
  (set-syntax-table rackton-mode-syntax-table)
  (setq-local comint-prompt-regexp rackton-repl-prompt-regexp)
  (setq-local comint-prompt-read-only t)
  ;; Sent input keeps its font-lock fontification; comint would
  ;; otherwise stamp it with the comint-highlight-input face, which
  ;; shadows the syntax highlighting.
  (setq-local comint-highlight-input nil)
  (setq-local indent-tabs-mode nil)
  (setq-local lisp-indent-function #'rackton--indent-function)
  (setq-local indent-line-function #'rackton-repl--indent-line)
  ;; TAB indents, then completes the name at point (see
  ;; `rackton-tab-always-indent'); the capf below answers the query.
  (setq-local tab-always-indent rackton-tab-always-indent)
  (add-hook 'completion-at-point-functions
            #'rackton-repl-completion-at-point nil t)
  ;; The piped REPL answers every continuation line of a multi-line
  ;; form with a "..> " prompt; in a comint buffer they are noise.
  (add-hook 'comint-preoutput-filter-functions
            #'rackton-repl--strip-continuations nil t)
  ;; Separate interactions with a blank line before each prompt.  Runs
  ;; after stripping (APPEND), so continuation prompts are already gone.
  (add-hook 'comint-preoutput-filter-functions
            #'rackton-repl--blank-before-prompts t t)
  ;; The language's keywords describe Rackton source, so they must fire
  ;; only on what the user types.  Process output — the banner and the
  ;; prose of ,info/,type/,source replies — is not code; fontifying it
  ;; as code produces nonsense (a reply's "(class)" read as a keyword, a
  ;; type name in a signature read as a constructor).  Comint already
  ;; tags output with the `field' property `output', so the wrapped
  ;; matchers can skip it.  (The reply's *type* regions are a separate
  ;; rule below: type names there, not code.)
  (font-lock-add-keywords
   nil (mapcar (lambda (kw)
                 (cons (rackton-repl--matcher-where (car kw) #'rackton-repl--code-at-p)
                       (cdr kw)))
               rackton-font-lock-keywords))
  ;; The expected:/got: detail is pure type information, so every
  ;; capitalized name there is a type — never a data constructor, the
  ;; way the positional classifier would read one in expression
  ;; position.
  (font-lock-add-keywords
   nil `((,(rackton-repl--matcher-where rackton--type-name-regexp
                                        #'rackton-repl--type-detail-at-p)
          0 'font-lock-type-face)))
  ;; The ,type/,accepts/,search/,info replies carry type expressions —
  ;; the schemes right of `::', their wrapped continuations, and ,info's
  ;; bare instance/superprotocol heads.  Like the error type detail,
  ;; every capitalized name there is a type.
  (font-lock-add-keywords
   nil `((,(rackton-repl--matcher-where rackton--type-name-regexp
                                        #'rackton-repl--reply-type-at-p)
          0 'font-lock-type-face)))
  ;; A ,info reply on a sum type heads each `constructors:' line with a
  ;; data constructor.  The head left of `::' there is a constructor —
  ;; not a type (it names a value) and not a method (a protocol's
  ;; `methods:' lines share the shape but name functions).  Added after
  ;; the reply-type rule so it prepends ahead of it and wins for the head.
  (font-lock-add-keywords
   nil `((,(rackton-repl--matcher-where rackton--type-name-regexp
                                        #'rackton-repl--constructor-head-at-p)
          0 'rackton-constructor-face)))
  ;; The quantifier heading a scheme is a keyword, not a type.  Added
  ;; after the type-name rule so — since `font-lock-add-keywords' with a
  ;; nil HOW prepends — it sits ahead of it and wins for the capitalized
  ;; `All'.
  (font-lock-add-keywords
   nil `((,(rackton-repl--matcher-where rackton--quantifier-regexp
                                        #'rackton-repl--reply-type-at-p)
          0 'font-lock-keyword-face)))
  ;; Error first lines and labels are output, so they sidestep the
  ;; wrapping above and are highlighted wherever they appear.
  (font-lock-add-keywords nil rackton-repl--error-font-lock-keywords)
  ;; A `,'-prefixed meta-command leading the prompt: anchored on the
  ;; prompt itself, so it is installed unwrapped (the match begins on
  ;; the prompt, which `rackton-repl--code-at-p' would reject).
  (font-lock-add-keywords nil rackton-repl--command-font-lock-keywords))

(define-key inferior-rackton-mode-map (kbd "RET") #'rackton-repl-return)
(define-key inferior-rackton-mode-map (kbd "M-p") #'rackton-repl-previous-input)
(define-key inferior-rackton-mode-map (kbd "M-n") #'rackton-repl-next-input)

(defconst rackton-repl--continuation-regexp "\\.\\.> "
  "The piped REPL's continuation prompt.")

(defun rackton-repl--strip-continuations (output)
  "Remove the REPL's ..> continuation prompts from OUTPUT."
  (replace-regexp-in-string rackton-repl--continuation-regexp "" output))

(defun rackton-repl--blank-before-prompts (output)
  "Prepend a newline to each line-starting prompt in OUTPUT.
The preceding output already ends a line, so the inserted newline
shows as a blank line, visually separating successive interactions."
  (replace-regexp-in-string rackton-repl-prompt-regexp "\n\\&" output))

(defun rackton-repl--input-complete-p (input)
  "Non-nil when INPUT has no unclosed parenthesis or string."
  (with-temp-buffer
    (set-syntax-table rackton-mode-syntax-table)
    (insert input)
    (let ((state (parse-partial-sexp (point-min) (point-max))))
      (and (<= (car state) 0)          ; no unclosed parens
           (not (nth 3 state))))))     ; not inside a string

(defun rackton-repl--inside-sexp-p ()
  "Non-nil when point sits inside an open s-expression of the input.
Parses only the input region (from the process mark) so earlier
output in the buffer cannot skew the paren depth."
  (let* ((proc (get-buffer-process (current-buffer)))
         (start (and proc (marker-position (process-mark proc)))))
    (and start
         (>= (point) start)
         (> (car (parse-partial-sexp start (point))) 0))))

(defun rackton-repl--error-at-point ()
  "Parsed (FILE LINE COL) when point is on a Rackton error line, else nil.
LINE and COL are integers, as Rackton reports them (LINE 1-based, COL
0-based)."
  (save-excursion
    (forward-line 0)
    (when (looking-at rackton-repl--error-line-regexp)
      (list (match-string-no-properties 1)
            (string-to-number (match-string-no-properties 2))
            (string-to-number (match-string-no-properties 3))))))

(defun rackton-repl--visit-error (loc)
  "Visit error LOC — a list (FILE LINE COL) — in another window.
FILE is resolved against the REPL buffer's `default-directory' (where
the REPL process runs, so its relative paths match); LINE is 1-based
and COL 0-based."
  (let* ((file (nth 0 loc))
         (line (nth 1 loc))
         (col (nth 2 loc))
         (path (expand-file-name file)))
    (unless (file-exists-p path)
      (user-error "Cannot find error source: %s" path))
    ;; Select the window first, then move point: a buffer already shown
    ;; keeps its own window-point, which would override a point set
    ;; before its window is selected.
    (pop-to-buffer (find-file-noselect path))
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column col)))

(defun rackton-repl-visit-error-at-mouse (event)
  "Visit the error location of the line clicked in EVENT.
Bound in `rackton-repl-error-map', so a click on a highlighted error
line jumps to its source the way RET does."
  (interactive "e")
  (let ((posn (event-end event)))
    (with-current-buffer (window-buffer (posn-window posn))
      (save-excursion
        (goto-char (posn-point posn))
        (let ((err (rackton-repl--error-at-point)))
          (if err
              (rackton-repl--visit-error err)
            (user-error "No Rackton error at click")))))))

(defun rackton-repl-return ()
  "Visit an error, send the input, or open an indented line.
On a Rackton error line, jump to its source location, the way
compilation-mode's RET does.  Otherwise submit when the whole input is
complete and point is not inside an s-expression; else open a new line
so the form keeps growing."
  (interactive)
  (let ((err (rackton-repl--error-at-point)))
    (if err
        (rackton-repl--visit-error err)
      (let* ((proc (get-buffer-process (current-buffer)))
             (input (and proc
                         (buffer-substring-no-properties
                          (process-mark proc) (point-max)))))
        (if (and input
                 (rackton-repl--input-complete-p input)
                 (not (rackton-repl--inside-sexp-p)))
            (comint-send-input)
          (newline-and-indent))))))

;;; Layer 1: history navigation

(defvar-local rackton-repl--history-matching nil
  "Non-nil while a run of \\[rackton-repl-previous-input] prefix-matches.
The first press of a run picks plain cycling or prefix matching from the
cursor position; the rest of the run holds that choice, so repeated
presses keep cycling the same set.")

(defun rackton-repl--at-input-start-p ()
  "Non-nil when point is at the start of the REPL's editable input."
  (let ((proc (get-buffer-process (current-buffer))))
    (and proc (<= (point) (marker-position (process-mark proc))))))

(defun rackton-repl--history-move (n plain matching)
  "Move N steps through the input history, plain or prefix-matching.
PLAIN is the whole-history command (`comint-previous-input' or
`comint-next-input'); MATCHING is its prefix-matching counterpart.  The
mode is chosen on the first press of a run from the cursor position and
held for the rest of the run (`rackton-repl--history-matching'), so a
run of \\[rackton-repl-previous-input] / \\[rackton-repl-next-input]
keeps cycling the same set."
  (let ((continuing (memq last-command '(rackton-repl-previous-input
                                         rackton-repl-next-input))))
    (unless continuing
      (setq rackton-repl--history-matching
            (not (rackton-repl--at-input-start-p))))
    (if rackton-repl--history-matching
        ;; The comint matchers continue a run only when they see one of
        ;; themselves as `last-command'; this command sits between
        ;; presses, so spoof that once the run is under way.  Either
        ;; matcher name satisfies their shared check, so both directions
        ;; continue the same search.
        (let ((last-command (if continuing
                                'comint-previous-matching-input-from-input
                              last-command)))
          (funcall matching n))
      (funcall plain n))))

(defun rackton-repl-previous-input (n)
  "Recall the Nth previous input, matching the text before point.
At the very start of the input this is `comint-previous-input', which
cycles the whole history.  With text before point it is
`comint-previous-matching-input-from-input', which recalls the previous
input beginning with that text.  See `rackton-repl--history-move'."
  (interactive "p")
  (rackton-repl--history-move n #'comint-previous-input
                             #'comint-previous-matching-input-from-input))

(defun rackton-repl-next-input (n)
  "Recall the Nth next input, matching the text before point.
The forward mirror of `rackton-repl-previous-input': at the input start
`comint-next-input'; with text before point
`comint-next-matching-input-from-input'."
  (interactive "p")
  (rackton-repl--history-move n #'comint-next-input
                             #'comint-next-matching-input-from-input))

(defun rackton-repl--indent-line ()
  "Indent the current line of REPL input.
Narrows to the region from the prompt line's beginning, so the
indentation engine never scans backward into earlier process output
yet still counts real columns — the prompt is ordinary text on the
input's first line, and `λ> ' contains no delimiters to confuse the
parse."
  (let* ((proc (get-buffer-process (current-buffer)))
         (start (and proc (marker-position (process-mark proc))))
         ;; `forward-line', unlike `line-beginning-position', ignores
         ;; the prompt's field property and reaches the real line start.
         (prompt-bol (and start (save-excursion (goto-char start)
                                                (forward-line 0)
                                                (point))))
         (point-bol (save-excursion (forward-line 0) (point))))
    ;; Only continuation lines are indentable: the first input line
    ;; starts right after the (read-only) prompt.
    (if (and start
             (> (point) start)
             (> point-bol prompt-bol))
        (save-restriction
          (narrow-to-region prompt-bol (point-max))
          (lisp-indent-line))
      'noindent)))

(defun rackton-repl--buffer ()
  "The REPL buffer, or nil when none exists."
  (get-buffer rackton-repl--buffer-name))

(defun rackton-repl--process ()
  "The REPL process, or nil when none is running."
  (get-buffer-process rackton-repl--buffer-name))

(defun rackton-repl--live-p ()
  "Non-nil when the REPL process is running."
  (comint-check-proc rackton-repl--buffer-name))

(defun rackton-repl--ensure ()
  "Start the REPL unless it is already running; return its buffer."
  (let ((buf (get-buffer-create rackton-repl--buffer-name)))
    (unless (comint-check-proc buf)
      ;; A pipe, not a pty: over a pty the REPL believes it has a
      ;; terminal and runs its structural editor (cursor-control
      ;; escapes, keystroke echo); over a pipe it uses plain
      ;; line-by-line reading, which is what comint speaks.  NO_COLOR
      ;; additionally pins the output to plain text.
      (let ((process-connection-type nil)
            (process-environment (cons "NO_COLOR=1" process-environment)))
        (apply #'make-comint-in-buffer "rackton-repl" buf
               rackton-program nil rackton-repl-arguments))
      ;; Install the major mode only on a fresh buffer.  A killed REPL
      ;; (,q) leaves its buffer alive and fully set up; restarting it
      ;; reuses that buffer.  Re-running the mode there would call
      ;; `kill-all-local-variables', silently disabling any minor mode
      ;; the user had on (paredit) and duplicating the font-lock rules.
      (with-current-buffer buf
        (unless (derived-mode-p 'inferior-rackton-mode)
          (inferior-rackton-mode)))
      (rackton-repl--wait-for-prompt (get-buffer-process buf) 30))
    buf))

(defun rackton-repl--wait-for-prompt (proc timeout)
  "Wait up to TIMEOUT seconds for PROC's buffer to end at a prompt."
  (with-current-buffer (process-buffer proc)
    (let ((deadline (+ (float-time) timeout)))
      (while (and (process-live-p proc)
                  (< (float-time) deadline)
                  (not (save-excursion
                         (goto-char (point-max))
                         (forward-line 0)
                         (looking-at rackton-repl-prompt-regexp))))
        (accept-process-output proc 0.1)))))

;;;###autoload
(defun rackton-repl ()
  "Switch to the Rackton REPL, starting it first when necessary."
  (interactive)
  (pop-to-buffer (rackton-repl--ensure)))

(defvar rackton-repl--type-cache (make-hash-table :test #'equal)
  "Symbol name → type string (or `none'), invalidated on every send.")

;;; Layer 2: query channel
;;
;; One command/response exchange, invisible to the REPL buffer.  This
;; is the seam where future backends (LSP, debug server, hoogle
;; service) slot in: the UI commands below know only these functions.

(defun rackton-repl-query (input &optional timeout)
  "Send INPUT to the REPL; return the text it prints in reply.
The exchange runs through `comint-redirect', so the visible REPL
buffer stays untouched.  Waits up to TIMEOUT seconds (default 10)."
  (let ((proc (get-buffer-process (rackton-repl--ensure)))
        (out (generate-new-buffer " *rackton-query*")))
    (unless (string-prefix-p "," input)
      ;; Plain forms can (re)define names; cached types may be stale.
      (clrhash rackton-repl--type-cache))
    (unwind-protect
        (with-current-buffer (process-buffer proc)
          (comint-redirect-send-command-to-process input out proc nil t)
          (let ((deadline (+ (float-time) (or timeout 10))))
            (while (and (not comint-redirect-completed)
                        (< (float-time) deadline))
              (accept-process-output proc 0.1)))
          (unless comint-redirect-completed
            (comint-redirect-cleanup))
          (with-current-buffer out
            (string-trim
             (replace-regexp-in-string rackton-repl-prompt-regexp ""
                                       (buffer-string)))))
      (kill-buffer out))))

;;; Layer 3: sending code

(defun rackton-repl--send-form (form)
  "Send FORM to the REPL as if typed at its prompt."
  (let* ((buf (rackton-repl--ensure))
         (proc (get-buffer-process buf)))
    (clrhash rackton-repl--type-cache)
    (with-current-buffer buf
      (let* ((pmark (process-mark proc))
             ;; Preserve anything the user has half-typed at the prompt.
             (pending (buffer-substring pmark (point-max))))
        (delete-region pmark (point-max))
        (goto-char pmark)
        (insert form)
        (comint-send-input)
        (goto-char (point-max))
        (insert pending)))))

(defun rackton-repl--region-forms (beg end)
  "Return the top-level forms between BEG and END as strings.
Comments and any `#lang' line are skipped: the piped REPL reads
forms, not module headers."
  (save-excursion
    (goto-char beg)
    (let (forms)
      (while (progn
               (forward-comment (buffer-size))
               (when (looking-at "#lang[^\n]*")
                 (goto-char (match-end 0))
                 (forward-comment (buffer-size)))
               (< (point) end))
        (let ((start (point)))
          (condition-case nil
              (forward-sexp 1)
            (scan-error (goto-char end)))
          (when (> (point) start)
            (push (buffer-substring-no-properties start (point)) forms))))
      (nreverse forms))))

(defun rackton-send-region (beg end)
  "Send each top-level form between BEG and END to the REPL."
  (interactive "r")
  (mapc #'rackton-repl--send-form (rackton-repl--region-forms beg end)))

(defun rackton-eval-buffer ()
  "Send every top-level form in the buffer to the REPL."
  (interactive)
  (rackton-send-region (point-min) (point-max)))

(defun rackton-eval-defun ()
  "Send the top-level form around point to the REPL."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (rackton-send-region (point) end))))

(defun rackton-eval-last-sexp ()
  "Send the expression before point to the REPL."
  (interactive)
  (rackton-repl--send-form
   (buffer-substring-no-properties
    (save-excursion (backward-sexp) (point))
    (point))))

;;; Layer 3: queries

(defun rackton-repl--expr-for-query (prompt)
  "The active region, the sexp at point, or a string read with PROMPT."
  (cond ((use-region-p)
         (buffer-substring-no-properties (region-beginning) (region-end)))
        ((thing-at-point 'sexp t))
        (t (read-string prompt))))

(defun rackton-repl--show-doc (text)
  "Display TEXT in the *rackton-doc* buffer."
  (with-current-buffer (get-buffer-create "*rackton-doc*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text "\n")
      (goto-char (point-min)))
    (special-mode)
    (display-buffer (current-buffer))))

(defun rackton-repl--single-identifier-p (expr)
  "Non-nil when EXPR is one bare identifier (no whitespace or parens)."
  (let ((s (string-trim (or expr ""))))
    (and (> (length s) 0)
         (not (string-match-p "[ \t\n()]" s)))))

(defun rackton-repl--type-or-scheme (expr &optional timeout)
  "The `,type' reply for EXPR, else the `,info' declared scheme.
A bare identifier whose `,type' yields no type falls back to `,info':
a polymorphic, class-constrained binding (say `get-st', whose scheme is
\(All (s m) ((MonadState s m) => (m s)))) cannot be inferred at an
isolated call site, so its declared scheme is the type to show.  A
compound expression keeps its `,type' reply, error and all."
  (let ((reply (rackton-repl-query (concat ",type " expr) timeout)))
    (if (and (not (string-match-p "::" reply))
             (rackton-repl--single-identifier-p expr))
        (rackton-repl-query (concat ",info " expr) timeout)
      reply)))

(defun rackton-type (expr)
  "Show the inferred type of EXPR without evaluating it.
Interactively, EXPR is the region when active, else the sexp at point.
A bare identifier whose type cannot be inferred in isolation falls back
to its declared scheme (see `rackton-repl--type-or-scheme')."
  (interactive (list (rackton-repl--expr-for-query "Type of: ")))
  (message "%s" (rackton-repl--type-or-scheme expr)))

(defun rackton-repl--type-provider (name)
  "Provide NAME's type from a live REPL, or nil — a `rackton-type-functions'.
The fallback when no Language Server is connected: it answers only when
a REPL is already running, so the annotator never spawns one silently.
The `,type'/`,info' reply (see `rackton-repl--type-or-scheme') is read
down to the bare type by `rackton--scheme-type'."
  (when (rackton-repl--live-p)
    (rackton--scheme-type (rackton-repl--type-or-scheme name))))

(add-hook 'rackton-type-functions #'rackton-repl--type-provider t)

(defun rackton-describe-symbol (name)
  "Describe NAME — its scheme, class methods, or constructors."
  (interactive (list (or (thing-at-point 'symbol t)
                         (read-string "Describe: "))))
  (rackton-repl--show-doc (rackton-repl-query (concat ",info " name))))

(defun rackton-show-source (name)
  "Show the form that bound NAME in the session or prelude."
  (interactive (list (or (thing-at-point 'symbol t)
                         (read-string "Source of: "))))
  (rackton-repl--show-doc (rackton-repl-query (concat ",source " name))))

(defun rackton-accepts (type)
  "List functions and constructors accepting an argument of TYPE.
Hoogle-style search by argument position, answered by the REPL's
,accepts command (a richer search service is planned upstream)."
  (interactive "sAccepts type: ")
  (rackton-repl--show-doc (rackton-repl-query (concat ",accepts " type))))

(defun rackton-repl-search (signature)
  "Search the live session, imports, and prelude by SIGNATURE.
Hoogle-style whole-signature search answered by the REPL's ,search; a
string SIGNATURE searches names instead.  Unlike `rackton-search',
which reads the installed standard-library index from a shell, this
sees the session's own definitions."
  (interactive "sSearch signature: ")
  (rackton-repl--show-doc (rackton-repl-query (concat ",search " signature))))

(defun rackton-repl-returns (type)
  "List session, imported, and prelude functions returning TYPE.
Answered by the REPL's ,returns; sees the session's own definitions,
where the shell `rackton-search-returns' sees only the standard library."
  (interactive "sReturns type: ")
  (rackton-repl--show-doc (rackton-repl-query (concat ",returns " type))))

;;; Layer 3: clearing the display

(defun rackton-repl-clear-buffer ()
  "Erase the Rackton REPL's displayed output, keeping the session.
Everything above the current prompt is removed; the process and every
binding it holds are untouched.  Callable from any buffer."
  (interactive)
  (let ((buf (rackton-repl--buffer)))
    (unless (and buf (comint-check-proc buf))
      (user-error "No Rackton REPL is running"))
    (with-current-buffer buf
      (comint-clear-buffer))))

;;; Layer 3: resetting the session

(defun rackton-repl-reset ()
  "Reset the Rackton REPL session to a fresh prelude, after confirming.
Discards every definition, data type, class, and instance made since
the session began (Rackton's ,clear).  The displayed output is kept;
use `rackton-repl-clear-buffer' to erase that.  Sending the command
also clears the cached eldoc types, since the bindings they describe
are gone."
  (interactive)
  (unless (rackton-repl--live-p)
    (user-error "No Rackton REPL is running"))
  (when (y-or-n-p "Reset the Rackton session, discarding all definitions? ")
    (rackton-repl--send-form ",clear")))

;;; Layer 3: eldoc

(defun rackton-repl--type-of (name)
  "NAME's type per the REPL, or the symbol `none'.
Falls back to NAME's declared scheme when its type cannot be inferred
in isolation, so eldoc reports a polymorphic binding rather than going
silent (see `rackton-repl--type-or-scheme')."
  (let ((reply (rackton-repl--type-or-scheme name 2)))
    (if (string-match-p "::" reply) reply 'none)))

(defun rackton-repl-eldoc (callback &rest _)
  "Report the type of the symbol at point via CALLBACK.
For `eldoc-documentation-functions'.  Quiet unless the REPL is
already running — eldoc must never launch a process — and quiet when
eglot manages the buffer, so the LSP server's hover is then the single
source of type-at-point (and no REPL is needed for it)."
  (when (and (rackton-repl--live-p)
             (not (and (fboundp 'eglot-managed-p) (eglot-managed-p))))
    (when-let ((name (thing-at-point 'symbol t)))
      (unless (nth 8 (syntax-ppss))     ; strings and comments stay quiet
        (let ((doc (or (gethash name rackton-repl--type-cache)
                       (puthash name (rackton-repl--type-of name)
                                rackton-repl--type-cache))))
          (unless (eq doc 'none)
            (funcall callback doc :thing name)))))))

(defun rackton-repl--eldoc-setup ()
  "Hook `rackton-repl-eldoc' into eldoc for the current buffer."
  (add-hook 'eldoc-documentation-functions #'rackton-repl-eldoc nil t))

(add-hook 'rackton-mode-hook #'rackton-repl--eldoc-setup)

;;; Layer 3: completion

(defun rackton-repl--completions (prefix)
  "Names completing PREFIX, from the REPL's ,complete command.
Nil when the REPL offers none."
  (let ((reply (rackton-repl-query (concat ",complete " prefix) 2)))
    (and (not (string-empty-p reply))
         (split-string reply "\n" t))))

(defun rackton-repl-completion-at-point ()
  "Completion-at-point for Rackton names, answered by the REPL.
For `completion-at-point-functions', in both the REPL and source
buffers.  Quiet unless the REPL is running — completion must never
launch a process — and quiet when eglot manages the buffer, so its
LSP completion takes over.  Non-exclusive: when it offers nothing,
later functions (e.g. comint's filename completion) still run."
  (when (and (rackton-repl--live-p)
             (not (and (fboundp 'eglot-managed-p) (eglot-managed-p))))
    (let ((bounds (bounds-of-thing-at-point 'symbol)))
      (when bounds
        (let ((cands (rackton-repl--completions
                      (buffer-substring-no-properties (car bounds) (cdr bounds)))))
          (when cands
            (list (car bounds) (cdr bounds) cands :exclusive 'no)))))))

(defun rackton-repl--completion-setup ()
  "Hook `rackton-repl-completion-at-point' into the current buffer.
Appended, so eglot's LSP completion (when present) is consulted first."
  (add-hook 'completion-at-point-functions
            #'rackton-repl-completion-at-point t t))

(add-hook 'rackton-mode-hook #'rackton-repl--completion-setup)

;;; Keybindings layered onto the editing mode

(define-key rackton-mode-map (kbd "C-c C-z") #'rackton-repl)
(define-key rackton-mode-map (kbd "C-x C-e") #'rackton-eval-last-sexp)
(define-key rackton-mode-map (kbd "C-c C-c") #'rackton-eval-defun)
(define-key rackton-mode-map (kbd "C-c C-r") #'rackton-send-region)
(define-key rackton-mode-map (kbd "C-c C-k") #'rackton-eval-buffer)
(define-key rackton-mode-map (kbd "C-c C-t") #'rackton-type)
(define-key rackton-mode-map (kbd "C-c C-d") #'rackton-describe-symbol)
(define-key rackton-mode-map (kbd "C-c C-s") #'rackton-show-source)
(define-key rackton-mode-map (kbd "C-c C-a") #'rackton-accepts)
;; Session-aware search under the `C-c C-f' ("find") prefix, as the
;; Control variants of the stdlib searches `rackton-search' binds there.
(define-key rackton-mode-map (kbd "C-c C-f C-s") #'rackton-repl-search)
(define-key rackton-mode-map (kbd "C-c C-f C-r") #'rackton-repl-returns)
(define-key rackton-mode-map (kbd "C-c M-o") #'rackton-repl-clear-buffer)
(define-key inferior-rackton-mode-map (kbd "C-c M-o") #'rackton-repl-clear-buffer)
(define-key rackton-mode-map (kbd "C-c M-r") #'rackton-repl-reset)
(define-key inferior-rackton-mode-map (kbd "C-c M-r") #'rackton-repl-reset)

;;; Menu items layered onto the Rackton menu
;;
;; This layer's submenus sit ahead of the base "Go to Definition…"
;; item, so they appear in load order above it regardless of which
;; optional layers are present.  Session search lives under REPL: it is
;; a feature of the live session, and keeping it here means every
;; command appears in the submenu owned by the file that defines it.

(easy-menu-add-item rackton-mode-map '(menu-bar "Rackton")
  '("Evaluate"
    ["Last Sexp" rackton-eval-last-sexp :help "Evaluate the sexp before point"]
    ["Defun" rackton-eval-defun :help "Evaluate the top-level form at point"]
    ["Region" rackton-send-region :enable mark-active
     :help "Evaluate the region"]
    ["Buffer" rackton-eval-buffer :help "Evaluate the whole buffer"])
  "Go to Definition…")

(easy-menu-add-item rackton-mode-map '(menu-bar "Rackton")
  '("Inspect"
    ["Type" rackton-type :help "Show the type of the symbol at point"]
    ["Describe Symbol" rackton-describe-symbol
     :help "Describe the symbol at point"]
    ["Show Source" rackton-show-source
     :help "Visit the source of the symbol at point"]
    ["Accepts" rackton-accepts
     :help "Show what the symbol at point accepts"])
  "Go to Definition…")

(easy-menu-add-item rackton-mode-map '(menu-bar "Rackton")
  '("REPL"
    ["Start / Switch to REPL" rackton-repl :help "Start or switch to the REPL"]
    ["Clear REPL" rackton-repl-clear-buffer :help "Clear the REPL buffer"]
    ["Reset REPL" rackton-repl-reset :help "Restart the REPL process"]
    "--"
    ["Search Signature (session)" rackton-repl-search
     :help "Search the live session by signature"]
    ["Search Returns (session)" rackton-repl-returns
     :help "Search the live session by return type"])
  "Go to Definition…")

(provide 'rackton-repl)
;;; rackton-repl.el ends here
