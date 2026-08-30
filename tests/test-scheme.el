;;; test-scheme.el --- Tests for Scheme support  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  coopi

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for structural Scheme navigation and editing.

;;; Code:

(require 'combobulate)
(require 'combobulate-test-prelude)
(require 'scheme)

(defconst combobulate-test-scheme--datum-nodes
  '("boolean"
    "byte_vector"
    "character"
    "keyword"
    "list"
    "number"
    "quasiquote"
    "quasisyntax"
    "quote"
    "string"
    "symbol"
    "syntax"
    "unquote"
    "unquote_splicing"
    "unsyntax"
    "unsyntax_splicing"
    "vector")
  "Expected datum nodes for the supported Scheme grammar.")

(defconst combobulate-test-scheme--compound-datum-nodes
  '("byte_vector"
    "list"
    "quasiquote"
    "quasisyntax"
    "quote"
    "syntax"
    "unquote"
    "unquote_splicing"
    "unsyntax"
    "unsyntax_splicing"
    "vector")
  "Expected compound datum nodes for the supported Scheme grammar.")

(defun combobulate-test-scheme--goto (text)
  "Move point to the beginning of TEXT in the current buffer."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(defun combobulate-test-scheme--buffer-string ()
  "Return the current buffer without text properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest combobulate-test-scheme-classic-mode-registration ()
  "Classic `scheme-mode' resolves Scheme without an existing parser."
  (with-temp-buffer
    (scheme-mode)
    (should-not (treesit-parser-list))
    (should (eq (combobulate-primary-language t) 'scheme))))

(ert-deftest combobulate-test-scheme-registration ()
  "Scheme registers and activates its tree-sitter language."
  (combobulate-test (:language scheme :mode scheme-mode)
    "(define answer 42)"
    bob
    (should (eq (combobulate-primary-language) 'scheme))
    (should (combobulate-read minor-mode))))

(ert-deftest combobulate-test-scheme-datum-rules ()
  "Datum rules track every datum in the supported Scheme grammar."
  (combobulate-test (:language scheme :mode scheme-mode)
    "()"
    (should
     (equal
      (sort (combobulate-procedure-expand-rules
             combobulate-scheme--datum-rules)
            #'string<)
      (sort (copy-sequence combobulate-test-scheme--datum-nodes)
            #'string<)))))

(ert-deftest combobulate-test-scheme-compound-datum-rules ()
  "Compound rules track every nestable datum in the Scheme grammar."
  (combobulate-test (:language scheme :mode scheme-mode)
    "()"
    (should
     (equal
      (sort (combobulate-procedure-expand-rules
             combobulate-scheme--compound-datum-rules)
            #'string<)
      (sort (copy-sequence combobulate-test-scheme--compound-datum-nodes)
            #'string<)))))

(ert-deftest combobulate-test-scheme-datum-tree-shapes ()
  "The supported Scheme grammar parses every datum as expected."
  (combobulate-test (:language scheme :mode scheme-mode)
    "(#t #vu8(1 2) #\\a #:keyword (nested) 42 `(quasi) \
#`(quasisyntax) '(quoted) \"text\" symbol #'(syntax) ,unquote \
,@splice #,unsyntax #,@unsplice #(vector))"
    bob
    (let ((node (combobulate-node-at-point '("list"))))
      (should
       (equal
        (mapcar #'combobulate-node-type
                (combobulate-node-children node))
        combobulate-test-scheme--datum-nodes)))))

(ert-deftest combobulate-test-scheme-sibling-navigation-discards-trivia ()
  "Sibling navigation skips every structural trivia node."
  (combobulate-test (:language scheme :mode scheme-mode)
    "(alpha ; line comment
 #| block comment |# beta #;(ignored datum) gamma \
#!fold-case delta)"
    (combobulate-test-scheme--goto "alpha")
    (dolist (expected '("beta" "gamma" "delta"))
      (combobulate-navigate-next)
      (should (looking-at-p expected)))
    (dolist (expected '("gamma" "beta" "alpha"))
      (combobulate-navigate-previous)
      (should (looking-at-p expected)))))

(ert-deftest combobulate-test-scheme-logical-navigation ()
  "Logical navigation moves forward and backward between Scheme nodes."
  (combobulate-test (:language scheme :mode scheme-mode)
    "alpha beta gamma"
    bob
    (combobulate-navigate-logical-next)
    (should (looking-at-p "beta"))
    (combobulate-navigate-logical-next)
    (should (looking-at-p "gamma"))
    (combobulate-navigate-logical-previous)
    (should (looking-at-p "beta"))
    (combobulate-navigate-logical-previous)
    (should (looking-at-p "alpha"))))

(ert-deftest combobulate-test-scheme-hierarchy-up ()
  "Hierarchy navigation climbs through compound reader forms."
  (combobulate-test (:language scheme :mode scheme-mode)
    "#('(leaf))"
    (combobulate-test-scheme--goto "leaf")
    (combobulate-navigate-up)
    (should (looking-at-p "(leaf"))
    (combobulate-navigate-up)
    (should (looking-at-p "'(leaf"))
    (combobulate-navigate-up)
    (should (looking-at-p "#("))))

(ert-deftest combobulate-test-scheme-defun-procedure-is-structural ()
  "Only top-level datums satisfy Scheme defun procedures."
  (combobulate-test (:language scheme :mode scheme-mode)
    "(outer (inner leaf))\nnext"
    (with-navigation-nodes (:procedures (combobulate-read procedures-defun))
      (combobulate-test-scheme--goto "outer")
      (let ((outer (combobulate-node-parent
                    (combobulate-node-at-point))))
        (should (combobulate-nav-defun-node-p outer)))
      (combobulate-test-scheme--goto "inner")
      (let ((inner (combobulate-node-parent
                    (combobulate-node-at-point))))
        (should-not (combobulate-nav-defun-node-p inner)))
      (combobulate-test-scheme--goto "leaf")
      (should
       (equal (combobulate-node-text (combobulate-nav-get-defun))
              "(outer (inner leaf))")))))

(ert-deftest combobulate-test-scheme-defun-activation-contract ()
  "Defun identity uses activation rules without changing old node rules."
  (combobulate-test (:language scheme :mode scheme-mode)
    "(outer (inner leaf))"
    (combobulate-test-scheme--goto "inner")
    (let* ((inner (combobulate-node-parent
                   (combobulate-node-at-point)))
           (outer (combobulate-node-parent inner)))
      (let ((combobulate-default-procedures
             '((:activation-nodes ((:nodes ("list")))))))
        (should (combobulate-nav-defun-node-p inner))
        (should (combobulate-nav-defun-node-p outer)))
      (let ((combobulate-default-procedures
             '((:activation-nodes
                ((:nodes ("list") :has-parent ("program")))
                :selector
                (:choose node
                 :match-children (:match-rules ("number")))))))
        (should-not (combobulate-nav-defun-node-p inner))
        (should (combobulate-nav-defun-node-p outer))))))

(ert-deftest combobulate-test-scheme-defun-navigation ()
  "Defun navigation uses top-level datums from nested positions."
  (combobulate-test (:language scheme :mode scheme-mode)
    "first\n(outer (inner leaf))\nlast"
    (combobulate-test-scheme--goto "(outer")
    (let ((expected-start (point))
          (expected-end (scan-sexps (point) 1)))
      (combobulate-test-scheme--goto "leaf")
      (combobulate-navigate-beginning-of-defun)
      (should (= (point) expected-start))
      (combobulate-test-scheme--goto "leaf")
      (combobulate-navigate-end-of-defun)
      (should (= (point) expected-end)))))

(ert-deftest combobulate-test-scheme-mark-defun ()
  "Marking a defun selects the containing top-level datum."
  (combobulate-test (:language scheme :mode scheme-mode)
    "first\n(outer (inner leaf))\nlast"
    (combobulate-test-scheme--goto "(outer")
    (let ((expected-start (point))
          (expected-end (scan-sexps (point) 1)))
      (combobulate-test-scheme--goto "leaf")
      (combobulate-mark-defun)
      (should (= (region-beginning) expected-start))
      (should (= (region-end) expected-end)))))

(ert-deftest combobulate-test-scheme-sexp-navigation ()
  "S-expression navigation moves over complete reader datums."
  (combobulate-test (:language scheme :mode scheme-mode)
    "'(alpha #(beta gamma)) #vu8(1 2) omega"
    bob
    (let ((first-end (save-excursion
                       (search-forward "'(alpha #(beta gamma))"))))
      (combobulate-forward-sexp-function 1)
      (should (= (point) first-end)))
    (combobulate-skip-whitespace-forward t)
    (let ((second-start (point))
          (second-end (save-excursion
                        (search-forward "#vu8(1 2)"))))
      (combobulate-forward-sexp-function 1)
      (should (= (point) second-end))
      (combobulate-forward-sexp-function -1)
      (should (= (point) second-start)))))

(ert-deftest combobulate-test-scheme-transpose-sexps ()
  "Transposing swaps adjacent Scheme datums."
  (combobulate-test (:language scheme :mode scheme-mode)
    "(aaa bbb ccc)"
    (combobulate-test-scheme--goto "bbb")
    (combobulate-transpose-sexps)
    (should (equal (combobulate-test-scheme--buffer-string)
                   "(bbb aaa ccc)"))))

(ert-deftest combobulate-test-scheme-mark-node-dwim ()
  "DWIM marking selects the nearest Scheme datum first."
  (combobulate-test (:language scheme :mode scheme-mode)
    "(aaa bbb ccc)"
    (let ((combobulate-mark-node-or-thing-at-point nil))
      (combobulate-test-scheme--goto "bbb")
      (combobulate-mark-node-dwim 1 nil t)
      (should (equal (buffer-substring-no-properties
                      (region-beginning) (region-end))
                     "bbb")))))

(ert-deftest combobulate-test-scheme-splice-down ()
  "Splice-down preserves the selected datum and preceding siblings."
  (combobulate-test (:language scheme :mode scheme-mode)
    "(outer (inner aaa bbb ccc) tail)"
    (combobulate-test-scheme--goto "bbb")
    (combobulate-with-stubbed-proffer-choices (:choices '(0))
      (combobulate-splice-down))
    (should (equal (combobulate-test-scheme--buffer-string)
                   "(outer inner aaa bbb tail)"))))

(ert-deftest combobulate-test-scheme-splice-parent ()
  "Splice-parent preserves all children of the removed parent."
  (combobulate-test (:language scheme :mode scheme-mode)
    "(outer (inner aaa bbb ccc) tail)"
    (combobulate-test-scheme--goto "bbb")
    (combobulate-with-stubbed-proffer-choices (:choices '(0))
      (combobulate-splice-parent))
    (should (equal (combobulate-test-scheme--buffer-string)
                   "(outer inner aaa bbb ccc tail)"))))

(ert-deftest combobulate-test-scheme-parentheses-envelope-node ()
  "The default parentheses envelope wraps an applicable Scheme datum."
  (combobulate-test (:language scheme :mode scheme-mode)
    "alpha beta gamma"
    (combobulate-test-scheme--goto "beta")
    (let* ((node (combobulate-node-at-point '("symbol")))
           (envelope (combobulate-get-envelope-by-name "wrap-parentheses")))
      (should
       (seq-some
        (lambda (candidate)
          (combobulate-node-eq node candidate))
        (combobulate-envelope-get-applicable-nodes envelope)))
      (combobulate-execute-envelope "wrap-parentheses" node))
    (should (equal (combobulate-test-scheme--buffer-string)
                   "alpha (beta) gamma"))))

(ert-deftest combobulate-test-scheme-parentheses-envelope-region ()
  "The default parentheses envelope wraps an active Scheme region."
  (combobulate-test (:language scheme :mode scheme-mode)
    "alpha beta gamma delta"
    (combobulate-test-scheme--goto "beta")
    (set-mark (point))
    (search-forward "gamma")
    (activate-mark)
    (combobulate-execute-envelope "wrap-parentheses")
    (should (equal (combobulate-test-scheme--buffer-string)
                   "alpha (beta gamma) delta"))))

(provide 'test-scheme)
;;; test-scheme.el ends here
