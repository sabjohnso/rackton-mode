;;; rackton-dap.el --- dape integration for Rackton  -*- lexical-binding: t; -*-

;; Author: Samuel B. Johnson <samuel.bryant.johnson@gmail.com>
;; Version: 0.4.13
;; Package-Requires: ((emacs "28.1"))
;; Keywords: languages, tools

;;; Commentary:

;; Registers Rackton's Debug Adapter Protocol server,
;; `racket -l rackton/dap', with dape.  It provides breakpoints by
;; Rackton source line, stepping, stack frames, and locals under their
;; source names.  The DAP server needs the gui-debugger collection at
;; runtime (`raco pkg install drracket').
;;
;; The dependency on dape is optional and lazy: the config is added
;; only when dape itself loads, so requiring this file adds no hard
;; dependency.  Debug a buffer with `M-x dape' and pick `rackton'.
;;
;; The config carries `modes (rackton-mode)' so dape offers it
;; automatically in Rackton buffers, and `ensure dape-ensure-command'
;; so a missing Racket is reported clearly rather than as a launch
;; failure; `dape-buffer-default' makes the current file the program.

;;; Code:

(require 'rackton-mode)

(defvar dape-configs)                   ; defined by dape when it loads

(defun rackton-dap--register ()
  "Add the Rackton debug configuration to `dape-configs'.
Idempotent: replaces any existing `rackton' entry rather than adding a
second.  The launch command is built from `rackton-program'."
  (setf (alist-get 'rackton dape-configs)
        `(modes (rackton-mode)
          ensure dape-ensure-command
          command ,rackton-program
          command-args ("-l" "rackton/dap")
          :program dape-buffer-default)))

(with-eval-after-load 'dape (rackton-dap--register))

(provide 'rackton-dap)
;;; rackton-dap.el ends here
