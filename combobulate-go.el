;;; combobulate-go.el --- go support for combobulate  -*- lexical-binding: t; -*-

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

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-go nil
  "Configuration switches for GO"
  :group 'combobulate
  :prefix "combobulate-go-")

(defun combobulate-go-pretty-print-node-name (node default-name)
  "Pretty printer for Go nodes"
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("function_declaration"
       (concat "func "
               (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ("type_declaration"
       (concat "type "
               (thread-first node
                             (combobulate-node-child 0)
                             (combobulate-node-child-by-field "name")
                             (combobulate-node-text))))
      ("identifier" (combobulate-node-text node))
      (_ default-name)))
   40))

(defvar combobulate-go-definitions
  '((envelope-procedure-shorthand-alist
     '((general-statement
        . ((:activation-nodes
            ((:nodes ((rule "block") (rule "_statement")
                      (rule "source_file"))
                     :has-parent ("block" "source_file"))))))))
    (envelope-list
     '((:description
        "v, err := ... | if err != nil { ... }"
        :key "E"
        :mark-node t
        :shorthand general-statement
        :name "handle-error"
        :template
        ((p value "Value Variable") ", " (p err "Error Variable") " := " @ r> n>
         "if " (f err) " != nil {" n> @ n> "}" > n>))
       (:description
        "if ... { ... } [else { ... }]"
        :key "i"
        :mark-node t
        :shorthand general-statement
        :name "if-statement"
        :template
        ("if " @ (p true "Condition") " {" n>
         (choice* :missing
                  nil
                  :rest
                  (r> n>)
                  :name "if-block")
         "}" >
         (choice* :missing
                  nil
                  :rest
                  (" else {" n> @ r> n> "}" > n>)
                  :name "else-block")))
       (:description
        "for ... { ... }"
        :key "f"
        :mark-node t
        :shorthand general-statement
        :name "for-loop"
        :template
        ("for "
         (choice*
          :name "Range"
          :rest ("index, value := range " (p variable "Variable")))
         (choice*
          :name "Iterator"
          :rest ((p i "Iterator") " := 0; "
                 (choice* :name "<" :rest ((f i) " < " (p to "Max")))
                 (choice* :name "<=" :rest ((f i) " <= " (p to "Max")))
                 "; "
                 (f i) "++"))
         " {" n> @ r> n> "}" > n>))))
    (context-nodes
     '("identifier" "false" "true" "float_literal" "field_identifier" "type_identifier"))
    (indent-after-edit nil)
    (envelope-indent-region-function #'indent-region)
    (procedures-edit nil)
    (pretty-print-node-name-function #'combobulate-go-pretty-print-node-name)
    (procedures-sexp nil)
    (plausible-separators '(";" "," "\n"))
    (procedures-defun
     '((:activation-nodes ((:nodes ("function_declaration"))))))
    (procedures-logical
     '((:activation-nodes ((:nodes (all))))))
    (procedures-sibling
     `((:activation-nodes
        ((:nodes
          ((rule "literal_value")
           (rule "for_clause"))
          :has-parent ("literal_value" "for_clause")))
        :selector (:choose
                   parent
                   :match-children
                   (:discard-rules ("literal_value"))))
       (:activation-nodes
        ((:nodes ((rule "type_case"))
                 :position at
                 :has-parent ("type_case" ))
         (:nodes ((rule "block") (rule "source_file"))
                 :position at
                 :has-parent ("block" "source_file")))
        :selector (:choose parent :match-children t))
       ;; lists with declarations as immediate childre
       (:activation-nodes
        ((:nodes ((rx "declaration_list$"))))
        :selector (:choose
                   node
                   :match-children
                   (:match-rules (rx "_declaration$"))))
       (:activation-nodes
        ((:nodes ((rule "argument_list"))
                 :has-parent ("argument_list"))
         (:nodes ((rule "expression_list"))
                 :has-parent "expression_list"))
        :selector (:choose
                   parent
                   :match-children t))
       (:activation-nodes
        ((:nodes ((rx "statement$"))))
        :selector (:choose
                   node
                   :match-children t))
       (:activation-nodes
        ((:nodes ("import_spec_list")))
        :selector (:choose
                   node
                   :match-children t))
       (:activation-nodes
        ((:nodes ((rule "_statement"))))
        :selector (:choose
                   node
                   :match-children t))))
    (procedures-hierarchy
     `((:activation-nodes
        ((:nodes "block" :position at))
        :selector (:choose node :match-children t))
       (:activation-nodes
        ((:nodes "expression_case" at))
        :selector (:choose node :match-children t))
       (:activation-nodes
        ((:nodes ((rule "_statement")
                  (rule "_simple_statement")
                  (rule "source_file"))
                 :position at))
        :selector (:choose node :match-children
                           (:match-rules ("block"))))
       (:activation-nodes
        ((:nodes ((all))))
        :selector (:choose node :match-children t))))))

(define-combobulate-language
 :name go
 :language go
 :major-modes (go-mode go-ts-mode)
 :custom combobulate-go-definitions
 :setup-fn combobulate-go-setup)

(defun combobulate-go-setup (_))

(provide 'combobulate-go)
;;; combobulate-go.el ends here

