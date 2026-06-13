;;; rackton-repl.el --- Inferior REPL for the Rackton language  -*- lexical-binding: t; -*-

;; Author: Samuel B. Johnson <samuel.bryant.johnson@gmail.com>
;; Version: 0.4.3
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

(defcustom rackton-program "racket"
  "Program that hosts the Rackton REPL."
  :type 'string
  :group 'rackton)

(defcustom rackton-repl-arguments '("-l" "rackton/repl")
  "Arguments passed to `rackton-program' to boot the REPL."
  :type '(repeat string)
  :group 'rackton)

(defconst rackton-repl-prompt-regexp "^λ> *"
  "Regexp matching the Rackton REPL prompt.")

(defconst rackton-repl--buffer-name "*rackton-repl*")

;;; Layer 1: transport

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
  ;; The piped REPL answers every continuation line of a multi-line
  ;; form with a "..> " prompt; in a comint buffer they are noise.
  (add-hook 'comint-preoutput-filter-functions
            #'rackton-repl--strip-continuations nil t)
  (font-lock-add-keywords nil rackton-font-lock-keywords))

(define-key inferior-rackton-mode-map (kbd "RET") #'rackton-repl-return)

(defconst rackton-repl--continuation-regexp "\\.\\.> "
  "The piped REPL's continuation prompt.")

(defun rackton-repl--strip-continuations (output)
  "Remove the REPL's ..> continuation prompts from OUTPUT."
  (replace-regexp-in-string rackton-repl--continuation-regexp "" output))

(defun rackton-repl--input-complete-p (input)
  "Non-nil when INPUT has no unclosed parenthesis or string."
  (with-temp-buffer
    (set-syntax-table rackton-mode-syntax-table)
    (insert input)
    (let ((state (parse-partial-sexp (point-min) (point-max))))
      (and (<= (car state) 0)          ; no unclosed parens
           (not (nth 3 state))))))     ; not inside a string

(defun rackton-repl-return ()
  "Send the input when it is complete; otherwise open an indented line."
  (interactive)
  (let* ((proc (get-buffer-process (current-buffer)))
         (input (and proc
                     (buffer-substring-no-properties
                      (process-mark proc) (point-max)))))
    (if (and input (rackton-repl--input-complete-p input))
        (comint-send-input)
      (newline-and-indent))))

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
      (with-current-buffer buf
        (inferior-rackton-mode))
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

(defun rackton-type (expr)
  "Show the inferred type of EXPR without evaluating it.
Interactively, EXPR is the region when active, else the sexp at point."
  (interactive (list (rackton-repl--expr-for-query "Type of: ")))
  (message "%s" (rackton-repl-query (concat ",type " expr))))

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

;;; Layer 3: eldoc

(defun rackton-repl--type-of (name)
  "NAME's type per the REPL, or the symbol `none'."
  (let ((reply (rackton-repl-query (concat ",type " name) 2)))
    (if (string-match-p "::" reply) reply 'none)))

(defun rackton-repl-eldoc (callback &rest _)
  "Report the type of the symbol at point via CALLBACK.
For `eldoc-documentation-functions'.  Quiet unless the REPL is
already running — eldoc must never launch a process."
  (when (rackton-repl--live-p)
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

(provide 'rackton-repl)
;;; rackton-repl.el ends here
