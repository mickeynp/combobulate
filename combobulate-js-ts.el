;;; combobulate-javascript.el --- structured editing for JS+JSX and Typescript in combobulate  -*- lexical-binding: t; -*-

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

(defgroup combobulate-js-ts nil
  "Configuration switches for Javascript, Typescript and JSX/TSX."
  :group 'combobulate
  :prefix "combobulate-js-ts-")

(defcustom combobulate-js-ts-attribute-envelope-alist
  '(("className" . "attr-string")
    ("style" . "attr-expression-object")
    ("ref" . "attr-expression"))
  "Alist of envelopes to apply to certain JSX attributes when you press `='.

Where the car of the cell is the attribute identifier,
e.g. \"classname\", and the cdr is the type of envelope to
apply.

Two builtin ones include `attr-string', for string attributes;
and `attr-expression' for expression-based attributes."
  :group 'combobulate-js-ts
  :type '(alist :key-type string :value-type string))

(defcustom combobulate-js-ts-attribute-envelope-default "attr-string"
  "Default envelope to apply to a JSX attribute.

Only applied if `combobulate-js-ts-attribute-envelope-alist' does
not contain a valid JSX attribute alist entry."
  :group 'combobulate-js-ts
  :type 'string)

(defcustom combobulate-js-ts-enable-auto-close-tag t
  "Auto-close JSX tags when you type `>'."
  :group 'combobulate-js-ts
  :type 'boolean)

(defcustom combobulate-js-ts-enable-guess-close-tag t
  "Insert the closing tag for a corresponding open tag when you press `/'."
  :group 'combobulate-js-ts
  :type 'boolean)

(defcustom combobulate-js-ts-enable-attribute-envelopes t
  "Pick a sensible value for a JSX attribute when you type `='.

This uses `combobulate-js-ts-attribute-envelope-alist' to
determine the attribute and the corresponding envelope (sourced
from `combobulate-manipulation-envelopes') to insert."
  :group 'combobulate-js-ts
  :type 'boolean)

(defun combobulate-js-ts--get-function-name (node)
  (concat "function "
          (car (combobulate-query-node-text
                '((function_declaration) name: (_) @name)
                node t))))

(defun combobulate-js-ts--get-declaration-name (node)
  (concat "const "
          (car (combobulate-query-node-text
                '((lexical_declaration (variable_declarator name: (_) @name)))
                node t))))

(defun combobulate-javascript-pretty-print-node-name (node default-name)
  "Pretty printer for JS and JSX nodes"
  (cl-flet ((make-tag-text (node &optional before after)
              (concat "<" (or before "") (combobulate-node-text node) (or after "") ">"))
            (get-name (node) (combobulate-node-child-by-field node "name")))
    (pcase (combobulate-node-type node)
      ;; Turn JSX elements into something resembling an SGML-styled
      ;; tag as the default name is too generic to be useful.
      ("jsx_self_closing_element" (make-tag-text (get-name node) nil "/"))
      ("jsx_opening_element" (make-tag-text (get-name node)))
      ("jsx_closing_element" (make-tag-text (get-name node) "/"))
      ("jsx_element" (make-tag-text (get-name (combobulate-node-child-by-field node "open_tag"))))
      ("jsx_attribute" (combobulate-string-truncate
                        (concat (combobulate-node-text (combobulate-node-child node 0)) "="
                                (combobulate-node-text (combobulate-node-child node 1)))
                        40))
      ("function_declaration" (combobulate-js-ts--get-function-name node))
      ("lexical_declaration" (combobulate-js-ts--get-declaration-name node))
      (_ default-name))))

(defun combobulate-maybe-auto-close-tag ()
  "Insert `>' or maybe insert the closing tag."
  (interactive)
  (self-insert-command 1 ?>)
  (save-excursion
    (forward-char -1)
    (when-let (open-node (combobulate-node-at-point '("jsx_opening_element")))
      (forward-char 1)
      (insert (format "</%s>" (combobulate-node-text
                               (combobulate-node-child-by-field
                                open-node "name")))))))

(defun combobulate-maybe-close-tag-or-self-insert ()
  "Insert `/' or maybe close the nearest unopened tag."
  (interactive)
  (cl-flet ((get-text (node field)
              (thread-first node (combobulate-node-child-by-field field)
                            (combobulate-node-child-by-field "name")
                            (combobulate-node-text))))
    (if (looking-back "<" 1)
        (progn
          (let* ((element (combobulate-node-at-point '("jsx_element"))))
            (if (equal (get-text element "open_tag")
                       (get-text element "close_tag"))
                (self-insert-command 1 ?/)
              (combobulate-message "Closing node" element)
              (combobulate-pulse-node (combobulate-node-child-by-field element "open_tag"))
              (insert (format "/%s>" (get-text element "open_tag"))))))
      (self-insert-command 1 ?/))))



(defun combobulate-maybe-insert-attribute ()
  "Insert `=' or maybe a JSX attribute."
  (interactive)
  (let ((node (combobulate-node-at-point '("jsx_opening_element" "jsx_self_closing_element"))))
    (if (and node ;; (combobulate-point-in-node-range-p node)
             )
        (atomic-change-group
          (catch 'done
            (pcase-dolist (`(,attribute . ,envelope) combobulate-js-ts-attribute-envelope-alist)
              (when (looking-back (concat "\\<" attribute "\\>") (length attribute))
                (combobulate-apply-envelope (combobulate-get-envelope-by-name envelope) node t)
                (throw 'done t)))
            ;; catch flagrant, out-of-place uses of `='.
            (if (looking-back "[[:alpha:]]" 1)
                (combobulate-apply-envelope (combobulate-get-envelope-by-name
                                             combobulate-js-ts-attribute-envelope-default)
                                            node)
              (self-insert-command 1 ?=))))
      (self-insert-command 1 ?=))))

(defun combobulate-js-ts-setup (_)
  (when combobulate-js-ts-enable-attribute-envelopes
    (local-set-key (kbd "=") #'combobulate-maybe-insert-attribute))
  (when combobulate-js-ts-enable-guess-close-tag
    (local-set-key (kbd "/") #'combobulate-maybe-close-tag-or-self-insert))
  (when combobulate-js-ts-enable-auto-close-tag
    (local-set-key (kbd ">") #'combobulate-maybe-auto-close-tag))

  ;; NOTE This is subject to change
  (setq combobulate-manipulation-envelopes
        `((:description
           "( ... )"
           :key "("
           :extra-key "M-("
           :mark-node t
           :nodes ,(combobulate-production-rules-get "primary_expression")
           :name "wrap-parentheses"
           :template (@ "(" r ")"))
          (:description
           "<...> ... </...>"
           :name "tag"
           :mark-node t
           :nodes ("jsx_element" "jsx_self_closing_element" "jsx_fragment")
           :key "t"
           :template ("<" (p "Tag Name (blank for fragment): " tag) ">" > n>
                      r>
                      n> "</" (s tag) ">" >))
          (:description
           "{ ... }"
           :key "e"
           :nodes ("jsx_element" "jsx_self_closing_element" "jsx_fragment" "string")
           :name "expression"
           :mark-node t
           :template ("{" r "}"))
          (:description
           "{/* ... */}"
           :key ";"
           :nodes ("jsx_element" "jsx_self_closing_element" "jsx_fragment")
           :name "comment"
           :mark-node t
           :template ("{/*" r "*/}"))
          (:description
           "{... ? ... : ...}"
           :key "?"
           :mark-node t
           :nodes ("jsx_element" "jsx_self_closing_element" "jsx_fragment")
           :name "ternary"
           :template ("{" p "null" >
                      n > " ? " r> >
                      n > " : " "null" >
                      n > "}" >))
          (:description
           "...={ ... }"
           :key "=e"
           :mark-node nil
           :point-placement 'stay
           :nodes ("jsx_opening_element" "jsx_self_closing_element")
           :name "attr-expression"
           :template ("=" "{" @ "}"))
          (:description
           "...={{ ... }}"
           :key "=E"
           :mark-node nil
           :point-placement 'stay
           :nodes ("jsx_opening_element" "jsx_self_closing_element")
           :name "attr-expression-object"
           :template ("=" "{{" @ "}}"))
          (:description
           "...=\" ... \""
           :key "=s"
           :mark-node nil
           :point-placement 'stay
           :nodes ("jsx_opening_element" "jsx_self_closing_element")
           :name "attr-string"
           :template ("=" "\"" @ "\""))))

  (setq combobulate-pretty-print-node-name-function #'combobulate-javascript-pretty-print-node-name)
  (setq combobulate-manipulation-trim-whitespace 'backward)
  (setq combobulate-manipulation-trim-empty-lines t)
  (setq combobulate-manipulation-edit-procedures
        '((:activation-nodes
           ((:node
             ("named_imports" "object" "formal_parameters" "array" "object_type")
             :position at-or-in))
           :remove-types ("comment")
           :match-query ((_) (_)+ @match))
          (:activation-nodes
           ((:node
             "variable_declarator"
             :position at-or-in))
           :remove-types ("comment")
           :match-query ((_) name: (array_pattern (_)+  @match)))
          (:activation-nodes
           ((:node
             "jsx_attribute"
             :find-parent ("jsx_opening_element" "jsx_self_closing_element")
             :position at-or-in))
           :match-query ((_) (jsx_attribute)+ @match))
          ;; sibling-level editing
          (:activation-nodes
           ((:node
             ("jsx_self_closing_element" "jsx_expression" "jsx_element")
             :position at))
           :remove-types ("comment")
           :match-siblings (:keep-parent nil))
          ;; editing an element's opening/closing tag
          (:activation-nodes
           ((:node
             "jsx_element"
             :position in))
           :remove-types ("comment")
           :match-query (jsx_element (jsx_opening_element (identifier) @match)
                                     (jsx_closing_element (identifier) @match)))))
  (setq combobulate-navigation-sibling-skip-prefix t)
  (setq combobulate-navigation-sexp-nodes '("jsx_element"
                                            ;; NOTE: you could make a
                                            ;; legit argument that
                                            ;; you'd want to move by
                                            ;; opening/closing element
                                            ;; "jsx_opening_element"
                                            ;; "jsx_closing_element"
                                            "jsx_expression"
                                            ;; makes < and > in
                                            ;; typescript behave
                                            ;; as a cohesive
                                            ;; unit.
                                            "type_arguments"
                                            "jsx_attribute"
                                            "jsx_self_closing_element"))
  (setq combobulate-manipulation-splicing-procedures
        `((:activation-nodes
           ((:node
             ,(append
               '("jsx_fragment" "jsx_element" "jsx_self_closing_element" "jsx_expression"
                 "lexical_declaration")
               (combobulate-production-rules-get "if_statement")
               (combobulate-production-rules-get "declaration")
               (combobulate-production-rules-get "statement"))
             :find-base-rule-parent (:remove-types ("statement_block"))
             :position at-or-in))
           :match-siblings (:keep-parent nil :keep-siblings t))
          (:activation-nodes
           ((:node
             "pair"
             :find-parent ("pair")
             :position at-or-in))
           :match-query
           ((_) @discard (object ((_) ","? )+ @keep)))))
  ;; Required for the top-most splicing procedure: we remove
  ;; `statement_block' because it interferes with splicing. However,
  ;; it is also a key part in inferring relationships between certain
  ;; specialized node types like `lexical_declaration'. So this
  ;; snippet tweaks the inverted production rules so it recognizes the
  ;; right thing
  (combobulate-alist-set "lexical_declaration"
                         (list "statement" "for_statement")
                         combobulate-navigation-rules-overrides-inverted)

  (setq combobulate-navigation-drag-parent-nodes '("jsx_element" "jsx_expression"
                                                   "jsx_self_closing_element"
                                                   "pair"  "named_imports"
                                                   "object" "export_statement" "program"
                                                   "enum_body" "object_type"
                                                   "object_pattern" "array"
                                                   "switch_body" "switch_case"
                                                   "formal_parameters" "arguments"
                                                   "array_pattern" "statement_block"
                                                   "jsx_opening_element"))
  (combobulate-production-rules-set '("jsx_element" :remove-types ("jsx_text") :included-fields (:*unnamed*)))
  (combobulate-production-rules-set '("jsx_opening_element" :included-fields (:attribute)))
  (combobulate-production-rules-set '("jsx_self_closing_element" :included-fields (:attribute)))
  (combobulate-production-rules-set '("switch_case" :included-fields (:body)))
  (combobulate-production-rules-set '("export_statement" :included-fields (:*unnamed*)))
  (combobulate-production-rules-set '("program" :expand-rules (("statement" :all t) ("declaration" :all t))))
  (combobulate-production-rules-set '("statement_block" :expand-rules (("statement" :all t) ("declaration" :all t))))
  (dolist (node-type '("arguments" "array"))
    (combobulate-production-rules-set `(,node-type :keep-types ("member_expression")
                                                   :expand-rules (("expression" :all t)
                                                                  ("primary_expression" :all t)))))
  (combobulate-production-rules-set '("array_pattern" :expand-rules (("pattern" :all t))))

  (setq combobulate-navigation-defun-nodes '("arrow_function" "function_declaration"
                                             "class_declaration" "method_definition"))

  (setq combobulate-navigation-sibling-procedures
        `(;; for lists, arrays, objects, etc.
          (:activation-nodes
           ((:node
             ,(append
               (combobulate-production-rules-get "object_pattern")
               (combobulate-production-rules-get "object_type")
               (combobulate-production-rules-get "object")
               (combobulate-production-rules-get "expression")
               (combobulate-production-rules-get "primary_expression"))
             :position at-or-in
             :find-immediate-parent ("object" "object_type" "import_specifier" "object_pattern" "array" "arguments")))
           :remove-types ("comment")
           :match-children t)
          ;; for jsx
          (:activation-nodes
           ((:node
             ;; attributes can only appear in particular JSX elements,
             ;; so we need a distinct rule for them.
             ("jsx_attribute")
             :position at-or-in
             :find-immediate-parent
             ("jsx_opening_element" "jsx_self_closing_element")))
           :match-children (:all nil :keep-types ("jsx_attribute"))
           :remove-types ("comment"))
          (:activation-nodes
           ((:node
             ,(seq-difference (combobulate-production-rules-get "jsx_element")
                              '("jsx_closing_element" "jsx_opening_element" "jsx_text"))
             :position at-or-in
             :find-immediate-parent
             ,(seq-difference (combobulate-production-rules-get "jsx_element")
                              '("jsx_text" "jsx_closing_element" "jsx_opening_element"))))
           :remove-types ("comment" "jsx_text" "jsx_closing_element" "jsx_opening_element")
           :match-children t)
          ;;; for general navigation
          (:activation-nodes
           ((:node
             ,(append (combobulate-production-rules-get "object")
                      (combobulate-production-rules-get "statement")
                      (combobulate-production-rules-get "declaration"))
             :position at-or-in
             :find-immediate-parent ("program" "statement_block")))
           :remove-types ("comment")
           :match-children t)))

  (setq combobulate-navigation-parent-child-nodes
        '("program" "arrow_function"
          "function_declaration" "lexical_declaration"
          "export_statement" "jsx_fragment" "jsx_element"
          "jsx_expression" "object" "jsx_self_closing_element" "array"))

  (setq combobulate-navigation-default-nodes
        (append combobulate-navigation-parent-child-nodes
                `("jsx_attribute" "ternary_expression" "type_arguments" "string"
                  "arrow_function"
                  ,@(combobulate-production-rules-get "primary_expression")
                  ,@(combobulate-production-rules-get "object")
                  ,@(combobulate-production-rules-get "statement")
                  ,@(combobulate-production-rules-get "declaration"))))
  (setq combobulate-navigation-logical-nodes
        (append combobulate-navigation-default-nodes
                '("array" "statement_block" "import_statement"
                  "type_annotation" "lexical_declaration"
                  "jsx_opening_element" "jsx_expression"
                  "jsx_text"
                  "jsx_closing_element" "pair" "jsx_attribute"
                  "jsx_self_closing_element"))))


(provide 'combobulate-js-ts)
;;; combobulate-javascript.el ends here
