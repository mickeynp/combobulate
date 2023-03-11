;;; combobulate-c.el --- structured editing for C++ in combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-23  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
;; Package-Requires: ((emacs "29"))
;; Version: 0.1
;; Homepage: https://www.github.com/mickeynp/combobulate
;; Keywords: convenience, tools, languages

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
(require 'combobulate-interface)
(require 'combobulate-rules)
(require 'combobulate-html)

(defgroup combobulate-c nil
  "Configuration switches for C++."
  :group 'combobulate
  :prefix "combobulate-c-")

(defun combobulate-c-setup (_)
  ;; NOTE This is subject to change
  (setq combobulate-manipulation-envelopes
        `((:description
           "( ... )"
           :key "("
           :extra-key "M-("
           :mark-node t
           :nodes ,(combobulate-production-rules-get "_expression")
           :name "wrap-parentheses"
           :template (@ "(" r ")"))
          (:description
           "{ ... }"
           :key "e"
           :nodes ("expression_statement" "unary_expression")
           :name "expression"
           :mark-node t
           :template ("{" r "}"))
          (:description
           "{/* ... */}"
           :key ";"
           :nodes ("expression_statement" "unary_expression")
           :name "comment"
           :mark-node t
           :template ("{/*" r "*/}"))
          (:description
           "{... ? ... : ...}"
           :key "?"
           :mark-node t
           :nodes ("expression_statement" "unary_expression")
           :name "ternary"
           :template ("{" @ "null" >
                      n > " ? " @ r>
                      n > " : " "null" >
                      n > "}" >))))

  (setq combobulate-manipulation-trim-whitespace 'backward)
  (setq combobulate-manipulation-trim-empty-lines t)
  (setq combobulate-manipulation-edit-procedures
        '(;; edit parameters in functions
          (:activation-nodes
           ((:node "function_definition" :position at-or-in))
           :match-query (function_definition (parameters (_)+ @match))
           :remove-types ("comment"))
          ;; edit elements in containers and blocks
          (:activation-nodes
           ((:node ("_expression" "initializer_list") :position at-or-in))
           :match-query ((_) (_)+ @match)
           ;; :match-children t
           :remove-types ("comment"))
          ;; edit arguments in calls
          (:activation-nodes
           ((:node "argument_list" :position at-or-in))
           :match-query ((argument_list) (_)+ @match)
           :remove-types ("comment"))
          (:activation-nodes
           ((:node "template_argument_list" :position at-or-in))
           :match-query ((argument_list) (_)+ @match)
           :remove-types ("comment"))))
  (setq combobulate-navigation-sibling-skip-prefix t)
  (setq combobulate-navigation-sexp-nodes '("function_definition" "function_declarator" "class_specifier" "struct_specifier" "type_definition" "template_function"))
  (setq combobulate-manipulation-splicing-procedures
        `((:activation-nodes
           ((:node
             ,(append
               (combobulate-production-rules-get "unary_expression")
               (combobulate-production-rules-get "_expression")
               (combobulate-production-rules-get "if_statement"))
             :find-base-rule-parent t
             :position at-or-in))
           :match-siblings (:keep-parent nil :keep-siblings t))))

  (setq combobulate-navigation-drag-parent-nodes
        '("if_statement" "while_statement" "do_statement" "for_statement" "function_definition" "function_declarator" "function_definition" "switch_statement" "class_specifier" "struct_specifier" "type_definition" "preproc_include" "template_function"))


  (combobulate-production-rules-set '("argument_list"
                                      :included-fields (:*unnamed*)
                                      :expand-rules (("unary_expression" :all t)
                                                     ("_expression" :all t))))
  (combobulate-production-rules-set '("function_definition"
                                      :included-fields (:body)))
  (combobulate-production-rules-set '("class_specifier"
                                      :included-fields (:body)))
  (combobulate-production-rules-set '("struct_specifier"
                                      :included-fields (:body)))
  (combobulate-production-rules-set '("type_definition"
                                      :included-fields (:body)))
  (combobulate-production-rules-set '("if_statement"
                                      :included-fields (:consequence :alternative)))
  (combobulate-production-rules-set '("for_statement"
                                      :included-fields (:body)))
  (combobulate-production-rules-set '("for_range_loop"
                                      :included-fields (:body)))
  (combobulate-production-rules-set '("do_statement"
                                      :included-fields (:body)))
  (combobulate-production-rules-set '("while_statement"
                                      :included-fields (:body)))
  (combobulate-production-rules-set '("switch_statement"
                                      :included-fields (:body)))
  (combobulate-production-rules-set '("preproc_include"
                                      :included-fields (:path)))

  (setq combobulate-navigation-defun-nodes '("function_definition" "function_declarator" "class_specifier"
                                             "struct_specifier" "template_method" "template_function"
                                             "lambda_expression" "type_definition"))

  (setq combobulate-navigation-sibling-procedures
        `(
          ;; for general navigation
          (:activation-nodes
           ((:node
             ,(append (combobulate-production-rules-get "unary_expression")
                      (combobulate-production-rules-get "declaration")
                      '("switch_statement"))
             :position at-or-in
             :find-immediate-parent ("case_statement")))
           :remove-types ("comment")
           :match-children t)))

  (setq combobulate-navigation-parent-child-nodes
        (append
         (combobulate-production-rules-get "declaration")
         (combobulate-production-rules-get "_abstract_declarator")
         (combobulate-production-rules-get "parameter_list")
         '("call_expression" "initializer_list" "char_literal")))
  (setq combobulate-navigation-logical-nodes
        (append
         (combobulate-production-rules-get "_expression")
         (combobulate-production-rules-get "unary_expression")
         combobulate-navigation-default-nodes))

  (setq combobulate-navigation-default-nodes
        (seq-uniq (append
                   combobulate-navigation-logical-nodes
                   combobulate-navigation-parent-child-nodes))))

(provide 'combobulate-c)
;;; combobulate-c.el ends here
