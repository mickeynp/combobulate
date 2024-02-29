;;; test-manipulation.el --- tests for manipulation routines  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
;; Keywords:

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

;;

;;; Code:



(require 'combobulate-test-prelude)

(ert-deftest combobulate-procedure-apply-:match-children+anonymous ()
  :tags '(combobulate procedure)
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/procedure/function.py")
    (goto-marker 2)
    (let ((node (combobulate-node-at-point '("identifier"))))
      ;; with anonymous and all child possible nodes (using :match-children)
      (should (equal
               (mapcar #'combobulate-node-type
                       (combobulate-procedure-result-matched-nodes
                        (combobulate-procedure-apply
                         '(:activation-nodes
                           ((:nodes
                             ("identifier")
                             :position any
                             :has-parent ((irule "parameter"))))
                           :selector (:choose parent :match-children
                                              (:match-rules t :anonymous t)))
                         node)))
               '("(" "identifier" "," "identifier" "," "typed_default_parameter" ")"))))))

(ert-deftest combobulate-procedure-apply-:match-query-all-children ()
  :tags '(combobulate procedure)
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/procedure/function.py")
    (goto-marker 2)
    (let ((node (combobulate-node-at-point '("identifier"))))
      ;; with all possible child nodes using :match-query
      (should (equal
               (mapcar #'combobulate-node-type
                       (combobulate-procedure-result-matched-nodes
                        (combobulate-procedure-apply
                         '(:activation-nodes
                           ((:nodes
                             ("identifier")
                             :position any
                             :has-parent ((irule "parameter"))))
                           :selector (:choose
                                      parent :match-query
                                      (:query (_ _ + @match)
                                              :engine combobulate)))
                         node)))
               '("(" "identifier" "," "identifier" "," "typed_default_parameter" ")"))))))

(ert-deftest combobulate-procedure-apply-:match-children-no-anonymous ()
  :tags '(combobulate procedure)
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/procedure/function.py")
    (goto-marker 2)
    (let ((node (combobulate-node-at-point '("identifier"))))
      ;; no anonymous
      (should (equal
               (mapcar #'combobulate-node-type
                       (combobulate-procedure-result-matched-nodes
                        (combobulate-procedure-apply
                         '(:activation-nodes
                           ((:nodes
                             ("identifier")
                             :position any
                             :has-parent ((irule "parameter"))))
                           :selector (:choose parent :match-children (:match-rules t :anonymous nil)))
                         node)))
               '("identifier" "identifier" "typed_default_parameter"))))))

(ert-deftest combobulate-procedure-apply-:match-children-keep-identifiers ()
  :tags '(combobulate procedure)
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/procedure/function.py")
    (goto-marker 2)
    (let ((node (combobulate-node-at-point '("identifier"))))
      ;; only identifiers
      (should (equal
               (mapcar #'combobulate-node-type
                       (combobulate-procedure-result-matched-nodes
                        (combobulate-procedure-apply
                         '(:activation-nodes
                           ((:nodes
                             ("identifier")
                             :position any
                             :has-parent ((irule "parameter"))))
                           :selector (:choose parent :match-children (:match-rules ("identifier") :anonymous nil)))
                         node)))
               '("identifier" "identifier"))))))

(ert-deftest combobulate-procedure-apply-:match-children-filter-identifiers ()
  :tags '(combobulate procedure)
  (combobulate-test (:language python :mode python-ts-mode :fixture "./fixtures/procedure/function.py")
    (goto-marker 2)
    (let ((node (combobulate-node-at-point '("identifier"))))
      ;; filter out identifiers
      (should (equal
               (mapcar #'combobulate-node-type
                       (combobulate-procedure-result-matched-nodes
                        (combobulate-procedure-apply
                         '(:activation-nodes
                           ((:nodes
                             ("identifier")
                             :position any
                             :has-parent ((irule "parameter"))))
                           :selector (:choose parent
                                              :match-children (:discard-rules ("identifier") :anonymous nil)))
                         node)))
               '("typed_default_parameter"))))))


(ert-deftest combobulate-procedure-apply-has-parent ()
  :tags '(combobulate procedure)
  ;; Simple string node test
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/procedure/component-jsx.tsx")
    (goto-marker 1)
    ;; is immediate parent
    (should (combobulate-procedure-apply-has-parent
             '("jsx_element") (combobulate-node-at-point '("jsx_expression"))))
    ;; is not a parent at all
    (should-not (combobulate-procedure-apply-has-parent
                 '("jsx_expression") (combobulate-node-at-point '("jsx_element"))))
    ;; is a parent, but not an immediate one
    (should-not (combobulate-procedure-apply-has-parent
                 '("arrow_function") (combobulate-node-at-point '("jsx_expression"))))))

(ert-deftest combobulate-procedure-apply-has-ancestor ()
  :tags '(combobulate procedure)
  ;; Simple string node test
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/procedure/component-jsx.tsx")
    (goto-marker 1)
    ;; is immediate parent
    (should (combobulate-procedure-apply-has-ancestor
             '("jsx_element") (combobulate-node-at-point '("jsx_expression"))))
    ;; is not a parent at all
    (should-not (combobulate-procedure-apply-has-ancestor
                 '("jsx_expression") (combobulate-node-at-point '("jsx_element"))))
    ;; is a parent, but not an immediate one, which is valid for an ancestor check
    (should (combobulate-procedure-apply-has-ancestor
             '("arrow_function") (combobulate-node-at-point '("jsx_expression"))))))

(ert-deftest combobulate-procedure-expand-rules-string ()
  :tags '(combobulate procedure)
  ;; Simple string node test
  (combobulate-test (:language tsx :mode tsx-ts-mode)
    (should-not (seq-difference (combobulate-procedure-expand-rules '("statement"))
                                '("statement")))))

(ert-deftest combobulate-procedure-apply-activation-nodes-has-parent ()
  :tags '(combobulate procedure)
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/procedure/component-jsx.tsx")
    (goto-marker 1)
    (let ((node (combobulate-node-at-point '("jsx_expression"))))
      ;; no match - no immediate parent with arrow_function
      (should-not (combobulate-procedure-apply-activation-nodes
                   '((:nodes ("jsx_expression") :position any :has-parent ("arrow_function")))
                   (combobulate-node-at-point '("jsx_expression"))))
      ;; does have a match - second nodes entry matches
      (should (equal
               (combobulate-procedure-result-parent-node
                (combobulate-procedure-apply-activation-nodes
                 '(;; no match
                   (:nodes ("jsx_expression") :position any :has-parent ("arrow_function"))
                   ;; this matches
                   (:nodes ("jsx_expression") :position any :has-parent ("jsx_element")))
                 node))
               (combobulate-node-parent node))))))


(ert-deftest combobulate-procedure-apply-activation-nodes-position ()
  :tags '(combobulate procedure)
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/procedure/component-jsx.tsx")
    (goto-marker 1)
    (let ((node (combobulate-node-at-point '("jsx_expression"))))
      ;; match: point is at the beginning.
      (should (equal
               (combobulate-procedure-result-parent-node
                (combobulate-procedure-apply-activation-nodes
                 '((:nodes ("jsx_expression") :position at :has-parent ("jsx_element")))
                 node))
               (combobulate-node-parent node)))
      ;; no match: point is inside the node
      (save-excursion
        (forward-char 1)
        (should-not (combobulate-procedure-apply-activation-nodes
                     '((:nodes ("jsx_expression") :position at :has-parent ("jsx_element")))
                     node)))
      ;; no match: point is before the node.
      (save-excursion
        (forward-char -1)
        (should-not (combobulate-procedure-apply-activation-nodes
                     '((:nodes ("jsx_expression") :position at :has-parent ("jsx_element")))
                     node)))
      ;; match: point is inside the node
      (save-excursion
        (forward-char 1)
        (should (equal
                 (combobulate-procedure-result-parent-node
                  (combobulate-procedure-apply-activation-nodes
                   '((:nodes ("jsx_expression") :position in :has-parent ("jsx_element")))
                   node))
                 (combobulate-node-parent node))))
      ;; match: point inside and then at the beginning.
      (save-excursion
        (should (equal
                 (combobulate-procedure-result-parent-node
                  (combobulate-procedure-apply-activation-nodes
                   '((:nodes ("jsx_expression") :position any :has-parent ("jsx_element")))
                   node))
                 (combobulate-node-parent node)))
        (forward-char 1)
        (should (equal
                 (combobulate-procedure-result-parent-node
                  (combobulate-procedure-apply-activation-nodes
                   '((:nodes ("jsx_expression") :position any :has-parent ("jsx_element")))
                   node))
                 (combobulate-node-parent node)))))))

(ert-deftest combobulate-procedure-apply-activation-nodes-has-ancestor ()
  :tags '(combobulate procedure)
  (combobulate-test (:language tsx :mode tsx-ts-mode :fixture "./fixtures/procedure/component-jsx.tsx")
    (goto-marker 1)
    (let ((node (combobulate-node-at-point '("jsx_expression"))))
      ;; no match - no ancestor at all with function_declaration
      (should-not (combobulate-procedure-apply-activation-nodes
                   '((:nodes ("jsx_expression") :position any :has-ancestor ("function_declaration")))
                   (combobulate-node-at-point '("jsx_expression"))))
      ;; picks the first match - arrow_function
      (should (equal
               (combobulate-procedure-result-parent-node
                (combobulate-procedure-apply-activation-nodes
                 '((:nodes ("jsx_expression") :position any :has-ancestor ("arrow_function"))
                   (:nodes ("jsx_expression") :position any :has-ancestor ("jsx_element")))
                 node))
               (combobulate-node-at-point '("arrow_function"))))
      ;; swap the order of the activation nodes
      (should (equal
               (combobulate-procedure-result-parent-node
                (combobulate-procedure-apply-activation-nodes
                 '((:nodes ("jsx_expression") :position any :has-ancestor ("jsx_element"))
                   (:nodes ("jsx_expression") :position any :has-ancestor ("arrow_function")))
                 node))
               (combobulate-node-at-point '("jsx_element")))))))

(ert-deftest combobulate-procedure-collect-activation-nodes ()
  :tags '(combobulate procedure)
  (combobulate-test (:language tsx :mode tsx-ts-mode)
    (should-not (seq-difference (combobulate-procedure-collect-activation-nodes
                                 '((:activation-nodes ((:nodes ("statement"))
                                                       (:nodes ("string"))))
                                   (:activation-nodes ((:nodes ("number"))))))
                                '("statement" "string" "number")))))


(ert-deftest combobulate-procedure-expand-rules-rule ()
  :tags '(combobulate procedure)
  (combobulate-test (:language tsx :mode tsx-ts-mode)
    (should-not (seq-difference (combobulate-procedure-expand-rules
                                 '("statement" (rule "statement")))
                                '("statement" "break_statement" "import_statement" "expression_statement" "do_statement"
                                  "with_statement" "if_statement" "continue_statement" "switch_statement" "try_statement"
                                  "ambient_declaration" "import_alias" "class_declaration" "type_alias_declaration"
                                  "interface_declaration" "function_signature" "variable_declaration"
                                  "generator_function_declaration" "internal_module" "enum_declaration"
                                  "module" "lexical_declaration" "abstract_class_declaration" "function_declaration"
                                  "empty_statement" "for_statement" "debugger_statement" "export_statement"
                                  "statement_block" "throw_statement" "while_statement" "return_statement"
                                  "labeled_statement" "for_in_statement")))))


(ert-deftest combobulate-procedure-expand-rules-irule ()
  :tags '(combobulate procedure)
  (combobulate-test (:language tsx :mode tsx-ts-mode)
    (should-not (seq-difference
                 (combobulate-procedure-expand-rules
                  '(irule "jsx_element"))
                 '("expression" "jsx_element" "jsx_attribute")))))

(ert-deftest combobulate-procedure-expand-rules-exclude ()
  :tags '(combobulate procedure)
  (combobulate-test (:language tsx :mode tsx-ts-mode)
    (should-not (seq-difference (combobulate-procedure-expand-rules
                                 '((exclude (rule "statement") "declaration" "expression_statement")))
                                '("break_statement" "import_statement" "do_statement" "with_statement"
                                  "if_statement" "continue_statement" "switch_statement" "try_statement"
                                  "ambient_declaration" "import_alias" "class_declaration" "type_alias_declaration"
                                  "interface_declaration" "function_signature" "variable_declaration" "generator_function_declaration"
                                  "internal_module" "enum_declaration" "module" "lexical_declaration" "abstract_class_declaration"
                                  "function_declaration" "empty_statement" "for_statement" "debugger_statement" "export_statement"
                                  "statement_block" "throw_statement" "while_statement" "return_statement" "labeled_statement"
                                  "for_in_statement")))))

(ert-deftest combobulate-procedure-expand-rules-exclude-symmetry ()
  :tags '(combobulate procedure)
  (combobulate-test (:language tsx :mode tsx-ts-mode)
    (should-not (combobulate-procedure-expand-rules
                 '((exclude (irule "statement") (irule "statement")))))
    (should-not (combobulate-procedure-expand-rules
                 '((exclude (rule "statement") (rule "statement")))))))


(provide 'test-manipulation)
;;; test-manipulation.el ends here
