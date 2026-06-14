;;; rackton-search-test.el --- Tests for rackton-search  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for the pure pieces (output parsing, button rendering)
;; plus one integration test that drives the real
;; `racket -l rackton/search' tool, skipped when it is unavailable.

;;; Code:

(require 'ert)
(require 'rackton-search)

(defconst rackton-search-test--sample
  (concat
   "concat-map :: (All (a b) (-> (-> a (List b)) (-> (List a) (List b))))\n"
   "    /home/sbj/Sandbox/rackton/data/list.rkt:43\n"
   "    /home/sbj/Sandbox/rackton/batteries.rkt\n"
   "const-map :: (All (a b f) ((Functor f) => (-> a (-> (f b) (f a)))))\n"
   "    /home/sbj/Sandbox/rackton/data/functor.rkt:10\n")
  "A representative chunk of `rackton/search' output.")

(ert-deftest rackton-search-parse-extracts-matches ()
  "The parser recovers name, signature, and located definitions."
  (let ((matches (rackton-search--parse rackton-search-test--sample)))
    (should (= 2 (length matches)))
    (let ((m (car matches)))
      (should (equal (plist-get m :name) "concat-map"))
      (should (string-prefix-p "(All (a b)" (plist-get m :signature)))
      (should (equal (plist-get m :locations)
                     '(("/home/sbj/Sandbox/rackton/data/list.rkt" . 43)
                       ("/home/sbj/Sandbox/rackton/batteries.rkt" . nil)))))
    (should (equal (plist-get (cadr matches) :name) "const-map"))))

(ert-deftest rackton-search-parse-tolerates-empty ()
  "Empty output yields no matches, not an error."
  (should (null (rackton-search--parse "")))
  (should (null (rackton-search--parse "No matches found.\n"))))

(ert-deftest rackton-search-render-buttonizes-locations ()
  "Each located definition becomes a button carrying its file and line."
  (with-temp-buffer
    (rackton-search--render
     '((:name "f" :signature "(-> a b)" :locations (("/tmp/x.rkt" . 7)))))
    (let ((button (next-button (point-min))))
      (should button)
      (should (equal (button-get button 'rackton-file) "/tmp/x.rkt"))
      (should (equal (button-get button 'rackton-line) 7)))))

(ert-deftest rackton-search-run-finds-known-name ()
  "Integration: the real tool finds a standard-library name."
  (skip-unless (executable-find "racket"))
  (let ((out (ignore-errors (rackton-search--run '("--name" "concat-map")))))
    (skip-unless (and out (string-match-p "::" out)))
    (should (string-match-p "concat-map ::" out))))

(ert-deftest rackton-search-binds-shell-keys ()
  "The stdlib search commands are on the C-c C-f prefix."
  (require 'rackton-mode)
  (should (eq (lookup-key rackton-mode-map (kbd "C-c C-f s")) 'rackton-search))
  (should (eq (lookup-key rackton-mode-map (kbd "C-c C-f r")) 'rackton-search-returns))
  (should (eq (lookup-key rackton-mode-map (kbd "C-c C-f a")) 'rackton-search-accepts))
  (should (eq (lookup-key rackton-mode-map (kbd "C-c C-f n")) 'rackton-search-name)))

(provide 'rackton-search-test)
;;; rackton-search-test.el ends here
