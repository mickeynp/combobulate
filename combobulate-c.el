;;; combobulate-c.el --- C support for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Tim McGilchrist

;; Author: Tim McGilchrist <timmcgil@gmail.com>
;; Keywords: languages, c

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
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-c nil
  "Configuration switches for C."
  :group 'combobulate
  :prefix "combobulate-c-")

(defun combobulate-c-pretty-print-node-name (node default-name)
  "Pretty printer for C nodes."
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("function_definition"
       (concat (or (combobulate-node-text
                    (combobulate-node-child-by-field node "declarator"))
                   "")
               "()"))
      ("struct_specifier"
       (concat "struct "
               (or (combobulate-node-text
                    (combobulate-node-child-by-field node "name"))
                   "")))
      ("enum_specifier"
       (concat "enum "
               (or (combobulate-node-text
                    (combobulate-node-child-by-field node "name"))
                   "")))
      ("union_specifier"
       (concat "union "
               (or (combobulate-node-text
                    (combobulate-node-child-by-field node "name"))
                   "")))
      ("type_definition"
       (concat "typedef "
               (or (combobulate-node-text
                    (combobulate-node-child-by-field node "declarator"))
                   "")))
      ("identifier" (combobulate-node-text node))
      (_ default-name)))
   40))

(eval-and-compile
  (defvar combobulate-c-definitions
    '((context-nodes
       '("identifier" "field_identifier" "type_identifier"
         "primitive_type" "number_literal" "string_literal"
         "char_literal" "true" "false" "null"))
      (envelope-procedure-shorthand-alist
       '((general-statement
          . ((:activation-nodes
              ((:nodes ((rule "compound_statement") (rule "statement")
                        (rule "translation_unit"))
                       :has-parent ("compound_statement" "translation_unit"))))))))
      (envelope-list
       '((:description
          "if (...) { ... } [else { ... }]"
          :key "i"
          :mark-node t
          :shorthand general-statement
          :name "if-statement"
          :template
          ("if (" @ (p true "Condition") ") {" n>
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
          "for (...; ...; ...) { ... }"
          :key "f"
          :mark-node t
          :shorthand general-statement
          :name "for-loop"
          :template
          ("for (" @ "; ; ) {" n>
           r> n> "}" > n>))
         (:description
          "while (...) { ... }"
          :key "w"
          :mark-node t
          :shorthand general-statement
          :name "while-loop"
          :template
          ("while (" @ (p true "Condition") ") {" n>
           r> n> "}" > n>))
         (:description
          "do { ... } while (...);"
          :key "d"
          :mark-node t
          :shorthand general-statement
          :name "do-while-loop"
          :template
          ("do {" n> r> n> "} while (" @ (p true "Condition") ");" n>))
         (:description
          "switch (...) { ... }"
          :key "s"
          :mark-node t
          :shorthand general-statement
          :name "switch-statement"
          :template
          ("switch (" @ (p expr "Expression") ") {" n>
           r> n> "}" > n>))))
      (indent-after-edit nil)
      (envelope-indent-region-function #'indent-region)
      (pretty-print-node-name-function #'combobulate-c-pretty-print-node-name)
      (procedures-edit nil)
      (procedures-sexp nil)
      (plausible-separators '(";" "," "\n"))
      ;; Top-level items, not just functions.
      (procedures-defun
       '((:activation-nodes
          ((:nodes ("function_definition" "struct_specifier" "enum_specifier"
                    "union_specifier" "type_definition" "declaration"
                    "preproc_ifdef" "preproc_if" "preproc_def"
                    "preproc_function_def" "preproc_include"))))))
      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))
      (procedures-sibling
       '(;; Switch case clauses
         (:activation-nodes
          ((:nodes ("case_statement")
                   :has-parent ("switch_statement")))
          :selector (:choose parent :match-children (:match-rules ("case_statement"))))
         ;; Statements in a block.
         (:activation-nodes
          ((:nodes ((rule "compound_statement"))
                   :position at
                   :has-parent ("compound_statement")))
          :selector (:choose parent :match-children t))
         ;; Declarations at file scope, including inside `#ifdef' arms.
         ;; `identifier' is the name a guard tests, not an item in it.
         ;; `expression_statement' and `ERROR' are parse debris left by a
         ;; macro tree-sitter cannot expand; `ERROR' can go once
         ;; https://github.com/tree-sitter/tree-sitter-c/pull/330 ships.
         (:activation-nodes
          ;; `comment' is a grammar `extra', in no production rule.
          ((:nodes ((rule "translation_unit") (rule "preproc_ifdef") (rule "preproc_if")
                    (rule "preproc_else") (rule "preproc_elif")
                    "comment")
                   :position at
                   ;; `#else' / `#elif' arms are containers of their own.
                   :has-parent ("translation_unit" "preproc_ifdef" "preproc_if"
                                "preproc_else" "preproc_elif")))
          :selector (:choose parent :match-children
                             (:discard-rules ("expression_statement" "identifier" "ERROR"))))
         ;; Inside a macro definition: its name, parameter list and body
         ;; are not navigation targets, so treat point anywhere in one as
         ;; being on the definition itself.
         (:activation-nodes
          ((:nodes ("identifier" "preproc_params" "preproc_arg")
                   :has-parent ("preproc_def" "preproc_function_def")))
          :selector (:choose parent :match-siblings t))
         ;; Struct/union/enum members
         (:activation-nodes
          ((:nodes ((rule "field_declaration_list"))
                   :has-parent ("field_declaration_list")))
          :selector (:choose parent :match-children t))
         (:activation-nodes
          ((:nodes ((rule "enumerator_list"))
                   :has-parent ("enumerator_list")))
          :selector (:choose parent :match-children t))
         ;; Function arguments and parameters
         (:activation-nodes
          ((:nodes ((rule "argument_list"))
                   :has-parent ("argument_list"))
           (:nodes ((rule "parameter_list"))
                   :has-parent ("parameter_list")))
          :selector (:choose parent :match-children t))
         ;; Initializer lists
         (:activation-nodes
          ((:nodes ((rule "initializer_list"))
                   :has-parent ("initializer_list")))
          :selector (:choose parent :match-children t))
         ;; Statements inside control flow bodies
         (:activation-nodes
          ((:nodes ((rule "statement"))
                   :has-parent ((rule "statement"))))
          :selector (:choose parent :match-children t))
         ;; Top-level declarations
         (:activation-nodes
          ((:nodes ((rule "translation_unit"))))
          :selector (:choose node :match-children t))))
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes "compound_statement" :position at))
          :selector (:choose node :match-children t))
         (:activation-nodes
          ((:nodes ("case_statement") :position at))
          :selector (:choose node :match-children t))
         (:activation-nodes
          ((:nodes ((rule "statement")
                    (rule "translation_unit"))
                   :position at))
          :selector (:choose node :match-children
                             (:match-rules ("compound_statement"))))
         ;; Into an `#ifdef' / `#else' arm, skipping the name it tests.
         (:activation-nodes
          ((:nodes ("preproc_ifdef" "preproc_if" "preproc_else" "preproc_elif")
                   :position at))
          :selector (:choose node :match-children
                             (:discard-rules ("identifier"))))
         (:activation-nodes
          ((:nodes ((all))))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name c
 :major-modes (c-mode c-ts-mode)
 :custom combobulate-c-definitions
 :setup-fn combobulate-c-setup)

(defun combobulate-c-setup (_))

(provide 'combobulate-c)
;;; combobulate-c.el ends here
