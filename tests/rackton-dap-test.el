;;; rackton-dap-test.el --- Tests for rackton-dap  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit test for the dape registration.  It exercises the pure
;; registration against a let-bound `dape-configs', so it runs without
;; dape installed and without launching a debugger.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'rackton-dap)

;; dape is not loaded in the batch suite; declare its config alist
;; special so the `let' bindings below are dynamic, as they would be
;; once dape itself defines the variable.
(defvar dape-configs)

(ert-deftest rackton-dap-registers-config ()
  "The registration adds a `rackton' debug config dape can launch."
  (let ((dape-configs nil))
    (rackton-dap--register)
    (let ((plist (alist-get 'rackton dape-configs)))
      (should plist)
      (should (equal (plist-get plist 'command) "racket"))
      (should (equal (plist-get plist 'command-args) '("-l" "rackton/dap")))
      (should (equal (plist-get plist 'modes) '(rackton-mode)))
      (should (plist-member plist :program)))))

(ert-deftest rackton-dap-register-is-idempotent ()
  "Re-registering replaces, never duplicates, the rackton entry."
  (let ((dape-configs nil))
    (rackton-dap--register)
    (rackton-dap--register)
    (should (= 1 (cl-count 'rackton dape-configs :key #'car-safe)))))

(provide 'rackton-dap-test)
;;; rackton-dap-test.el ends here
