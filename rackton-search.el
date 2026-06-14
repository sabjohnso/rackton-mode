;;; rackton-search.el --- Signature search for Rackton  -*- lexical-binding: t; -*-

;; Author: Samuel B. Johnson <samuel.bryant.johnson@gmail.com>
;; Version: 0.4.14
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages, tools

;;; Commentary:

;; Hoogle-style search over the installed Rackton standard library,
;; driven by the `racket -l rackton/search' tool.  Four entry points:
;;
;;   `rackton-search'         by type pattern, e.g. (-> (List a) Integer)
;;   `rackton-search-returns' by result type
;;   `rackton-search-accepts' by accepted argument type
;;   `rackton-search-name'    by name substring
;;
;; Results land in a `*rackton-search*' buffer where every "file:line"
;; is a button that visits the defining source.  Unlike the REPL's
;; ,accepts, the search needs no running session — it reads the
;; installed index directly.  (Because no typing environment is in
;; play, the index lists constrained matches even when no instance is
;; in scope; the REPL applies that filter, this does not.)
;;
;; The tool is launched through `rackton-program' (shared with the REPL
;; and the LSP/debug bridges); it carries no other dependency.

;;; Code:

(require 'rackton-mode)
(require 'button)
(require 'subr-x)

;;; Running the tool

(defun rackton-search--run (args)
  "Run the Rackton search tool with ARGS and return its output.
ARGS are the arguments following `--' — a list of strings."
  (with-output-to-string
    (apply #'call-process rackton-program nil standard-output nil
           "-l" "rackton/search" "--" args)))

;;; Parsing the output
;;
;; The tool prints, per match, a "NAME :: SIGNATURE" line at column
;; zero followed by indented location lines, each "FILE" or "FILE:LINE".

(defun rackton-search--add-location (match file line)
  "Append (FILE . LINE) to MATCH's :locations.
LINE is an integer or nil.  MATCH already holds a :locations key, so
the list grows in place."
  (plist-put match :locations
             (append (plist-get match :locations)
                     (list (cons file line)))))

(defun rackton-search--parse (output)
  "Parse Rackton search OUTPUT into a list of match plists.
Each plist has :name, :signature, and :locations — a list of
\(FILE . LINE) pairs, LINE nil when the tool gave only a file."
  (let (matches current)
    (dolist (line (split-string output "\n"))
      (cond
       ((string-match "\\`\\([^ \t].*?\\) :: \\(.*\\)\\'" line)
        (when current (push current matches))
        (setq current (list :name (match-string 1 line)
                            :signature (match-string 2 line)
                            :locations nil)))
       ((and current
             (string-match "\\`[ \t]+\\(.+\\):\\([0-9]+\\)[ \t]*\\'" line))
        (rackton-search--add-location current (match-string 1 line)
                                      (string-to-number (match-string 2 line))))
       ((and current
             (string-match "\\`[ \t]+\\([^ \t].*?\\)[ \t]*\\'" line))
        (rackton-search--add-location current (match-string 1 line) nil))))
    (when current (push current matches))
    (nreverse matches)))

;;; Rendering results

(defun rackton-search--visit (button)
  "Visit the file and line recorded on BUTTON."
  (let ((file (button-get button 'rackton-file))
        (line (button-get button 'rackton-line)))
    (find-file-other-window file)
    (when line
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun rackton-search--render (matches)
  "Insert MATCHES at point; each location is a button visiting it."
  (dolist (m matches)
    (insert (propertize (plist-get m :name) 'face 'font-lock-function-name-face)
            " :: "
            (propertize (plist-get m :signature) 'face 'font-lock-type-face)
            "\n")
    (dolist (loc (plist-get m :locations))
      (insert "    ")
      (insert-text-button
       (if (cdr loc) (format "%s:%d" (car loc) (cdr loc)) (car loc))
       'action #'rackton-search--visit
       'rackton-file (car loc)
       'rackton-line (cdr loc)
       'help-echo "Jump to this definition")
      (insert "\n"))
    (insert "\n")))

(define-derived-mode rackton-search-mode special-mode "Rackton-Search"
  "Major mode for Rackton signature-search results.")

(defun rackton-search--display (args header)
  "Run the search with ARGS and show the results under HEADER."
  (let* ((output (rackton-search--run args))
         (matches (rackton-search--parse output)))
    (with-current-buffer (get-buffer-create "*rackton-search*")
      (rackton-search-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert header "\n\n")
        (if matches
            (rackton-search--render matches)
          (insert (if (string-blank-p output) "No matches.\n" output))))
      (goto-char (point-min)))
    (display-buffer "*rackton-search*")))

;;; Commands

;;;###autoload
(defun rackton-search (pattern)
  "Search the Rackton standard library for signatures matching PATTERN.
PATTERN is a type pattern, e.g. (-> (List a) Integer)."
  (interactive "sSearch type pattern: ")
  (rackton-search--display (list pattern)
                           (format "Signatures matching: %s" pattern)))

;;;###autoload
(defun rackton-search-returns (type)
  "Search the Rackton standard library for functions returning TYPE."
  (interactive "sReturns type: ")
  (rackton-search--display (list "--returns" type)
                           (format "Returns: %s" type)))

;;;###autoload
(defun rackton-search-accepts (type)
  "Search for functions or constructors that accept an argument of TYPE."
  (interactive "sAccepts type: ")
  (rackton-search--display (list "--accepts" type)
                           (format "Accepts: %s" type)))

;;;###autoload
(defun rackton-search-name (name)
  "Search the Rackton standard library for names containing NAME."
  (interactive "sName contains: ")
  (rackton-search--display (list "--name" name)
                           (format "Name contains: %s" name)))

(provide 'rackton-search)
;;; rackton-search.el ends here
