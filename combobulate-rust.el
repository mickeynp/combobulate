;;; combobulate-rust.el --- rust support for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  cowboy2013

;; Author: cowboy2013 <enjoylife.czzz@gmail.com>
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
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-rust nil
  "Configuration switches for Rust."
  :group 'combobulate
  :prefix "combobulate-rust-")

(defun combobulate-rust-pretty-print-node-name (node default-name)
  "Pretty printer for Rust nodes."
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("function_item"
       (concat "fn "
               (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ("struct_item"
       (concat "struct "
               (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ("enum_item"
       (concat "enum "
               (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ("impl_item"
       (concat "impl "
               (combobulate-node-text (combobulate-node-child-by-field node "type"))))
      ("trait_item"
       (concat "trait "
               (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ("mod_item"
       (concat "mod "
               (combobulate-node-text (combobulate-node-child-by-field node "name"))))
      ("identifier" (combobulate-node-text node))
      ("field_identifier" (combobulate-node-text node))
      ("type_identifier" (combobulate-node-text node))
      (_ default-name)))
   40))

(eval-and-compile
  (defvar combobulate-rust-definitions
    '((envelope-procedure-shorthand-alist
       '((general-statement
          . ((:activation-nodes
              ((:nodes ((rule "block") (rule "source_file"))
                       :has-parent ("block" "source_file"))))))))
      (envelope-list
       '((:description
          "if ... { ... } [else { ... }]"
          :key "i"
          :mark-node t
          :shorthand general-statement
          :name "if-expression"
          :template
          ("if " @ (p condition "Condition") " {" n>
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
          "match ... { ... => ... }"
          :key "m"
          :mark-node t
          :shorthand general-statement
          :name "match-expression"
          :template
          ("match " @ (p value "Value") " {" n>
           (p pattern "Pattern") " => " @ r> n>
           "}" > n>))
         (:description
          "for ... in ... { ... }"
          :key "f"
          :mark-node t
          :shorthand general-statement
          :name "for-loop"
          :template
          ("for " (p pattern "Pattern") " in " @ (p iterable "Iterable") " {" n>
           @ r> n>
           "}" > n>))
         (:description
          "while ... { ... }"
          :key "w"
          :mark-node t
          :shorthand general-statement
          :name "while-loop"
          :template
          ("while " @ (p condition "Condition") " {" n>
           @ r> n>
           "}" > n>))
         (:description
          "loop { ... }"
          :key "l"
          :mark-node t
          :shorthand general-statement
          :name "loop-expression"
          :template
          ("loop {" n>
           @ r> n>
           "}" > n>))
         (:description
          "fn ...(...) { ... }"
          :key "F"
          :mark-node t
          :shorthand general-statement
          :name "function-item"
          :template
          ("fn " (p name "Name") "(" @ ")" " {" n>
           @ r> n>
           "}" > n>))
         (:description
          "let ... = ...;"
          :key "L"
          :mark-node t
          :shorthand general-statement
          :name "let-declaration"
          :template
          ("let " (p pattern "Pattern") " = " @ r> ";"))))
      (context-nodes
       '("identifier" "field_identifier" "type_identifier" "shorthand_field_identifier"
         "false" "true" "float_literal" "integer_literal" "char_literal" "string_literal"))
      (indent-after-edit nil)
      (envelope-indent-region-function #'indent-region)
      (procedures-edit nil)
      (pretty-print-node-name-function #'combobulate-rust-pretty-print-node-name)
      (procedures-sexp nil)
      (plausible-separators '(";" "," "\n"))
      (procedures-defun
       '((:activation-nodes
          ((:nodes ("function_item" "struct_item" "enum_item" "impl_item"
                    "trait_item" "mod_item" "const_item" "static_item"))))))
      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))
      (procedures-sibling
       `((:activation-nodes
          ((:nodes ((rule "block") (rule "source_file"))
                   :position at
                   :has-parent ("block" "source_file")))
          :selector (:choose parent :match-children t))
         (:activation-nodes
          ((:nodes ("match_block") :position at))
          :selector (:choose node :match-children t))
         (:activation-nodes
          ((:nodes ((rule "match_arm"))
                   :has-parent ("match_block")))
          :selector (:choose parent :match-children t))
         (:activation-nodes
          ((:nodes ((rx "declaration_list" eol))))
          :selector (:choose
                     node
                     :match-children
                     (:match-rules (rx ("function_item" "const_item" "static_item" "type_item") eol))))
         (:activation-nodes
          ((:nodes ((rx "field_declaration_list" eol))))
          :selector (:choose
                     node
                     :match-children
                     (:match-rules (rx "field_declaration" eol))))
         (:activation-nodes
          ((:nodes ((rule "arguments"))
                   :has-parent ("arguments"))
           (:nodes ((rule "parameters"))
                   :has-parent "parameters"))
          :selector (:choose
                     parent
                     :match-children t))
         (:activation-nodes
          ((:nodes ("use_list" "scoped_use_list" "enum_variant_list")))
          :selector (:choose
                     node
                     :match-children t))
         (:activation-nodes
          ((:nodes ((rule "source_file"))
                   :position at
                   :has-parent ("source_file")))
          :selector (:choose parent :match-children t))))
      (procedures-hierarchy
       `((:activation-nodes
          ((:nodes "block" :position at))
          :selector (:choose node :match-children t))
         (:activation-nodes
          ((:nodes ("match_block") :position at))
          :selector (:choose node :match-children t))
         (:activation-nodes
          ((:nodes ((rule "source_file"))
                   :position at))
          :selector (:choose node :match-children
                             (:match-rules ("block"))))
         (:activation-nodes
          ((:nodes ((all))))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name rust
 :major-modes (rust-ts-mode)
 :custom combobulate-rust-definitions
 :setup-fn combobulate-rust-setup)

(defun combobulate-rust-setup (_))

(provide 'combobulate-rust)
;;; combobulate-rust.el ends here
