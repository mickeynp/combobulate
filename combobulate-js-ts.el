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
(require 'combobulate-html)



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

(defun combobulate-js-ts-pretty-print (node default-name)
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
      ("jsx_element" (concat
                      (make-tag-text
                       (get-name (combobulate-node-child-by-field node "open_tag")))))
      ("jsx_attribute" (combobulate-string-truncate
                        (concat (combobulate-node-text (combobulate-node-child node 0)) "="
                                (combobulate-node-text (combobulate-node-child node 1)))
                        40))
      ("function_declaration" (combobulate-js-ts--get-function-name node))
      ("lexical_declaration" (combobulate-js-ts--get-declaration-name node))
      (_ default-name))))

(eval-and-compile
  (defvar combobulate-jsx-definitions
    '((context-nodes
       '("string_literal" "identifier"
         "nested_identifier" "property_identifier"
         "shorthand_property_identifier_pattern"
         "string_fragment" "number"))
      ;; NOTE This is subject to change
      (envelope-procedure-shorthand-alist
       '((valid-jsx-expression
          . ((:activation-nodes
              ((:nodes
                (exclude ("jsx_element" "jsx_text" "jsx_self_closing_element"
                          "jsx_fragment" "jsx_attribute" (rule "jsx_attribute"))
                         "jsx_expression")
                :has-parent (irule "jsx_expression"))))))
         (expressions
          . ((:activation-nodes
              ((:nodes ((rule "primary_expression") (rule "expression")))))))
         (wrap-expressions
          . ((:activation-nodes
              ((:nodes ((rule "primary_expression") (rule "expression")))))))
         (inside-arrow
          . ((:activation-nodes
              ((:nodes (rule "arrow_function" :body) :has-parent "arrow_function")))))
         (general-statement
          . ((:activation-nodes ((:nodes (rule "statement") :has-parent (irule "statement"))))))))
      (envelope-list
       '((:description
          "const [...] = useState(...)"
          :key "r s"
          :name "useState"
          :template ("const"
                     " [" (p State "State") ", "
                     "set" (f State capitalize)
                     "] = useState(" @ (p null "Value") ")"))
         (:description
          "useEffect(() => ..., [...])"
          :key "r e"
          :name "useEffect"
          :template (
                     > "useEffect(() => {" n>
                     > @ n
                     > "}, [" @ "])"))
         (:description
          "useCallback(..., [...])"
          :key "r c"
          :nodes ("function_declaration" "arrow_function" "generator_function_declaration")
          :mark-node t
          :name "useCallback"
          :template (
                     > "useCallback(" n>
                     > r> n
                     > ", [" @ "])"))
         ;; useMemo inside a jsx_expression also
         (:description
          "useMemo(..., [...])"
          :key "r M"
          :nodes ("function_declaration" "arrow_function" "generator_function_declaration" "jsx_element" "jsx_self_closing_element")
          :mark-node t
          :name "useMemo"
          :template ("useMemo(() => " > r> ", [" @ "])"))
         (:description
          "const ... = useCallback(..., [...])"
          :key "r C"
          :nodes ("function_declaration" "arrow_function" "generator_function_declaration")
          :mark-node t
          :name "useCallback-const"
          :template (
                     > "const " (p cachedFn "Cached Name") " = useCallback(() => {" n>
                     > r> n
                     > "}, [" @ "])"))
         (:description
          "<...> ... </...>"
          :name "tag"
          :mark-node t
          :nodes ("jsx_element" "jsx_expression" "jsx_self_closing_element" "jsx_fragment" "jsx_text")
          :key "t"
          :template ("<" (p tag "Tag Name: ") ">" n>
                     @ r>
                     n "</" (field tag) ">" >))
         (:description
          "<> ... </>"
          :name "fragment"
          :mark-node t
          :nodes ("jsx_element" "jsx_self_closing_element" "jsx_fragment")
          :key "f"
          :template ("<>" n>
                     r>
                     n> "</>"))
         (:description
          "if ( ... ) { ... }"
          :key "i"
          :shorthand general-statement
          :name "if-statement"
          :mark-node t
          :template ("if " "(" @ ")" " " "{" n>  r> n> "}"))
         (:description
          "{ ... }"
          :key "e"
          :shorthand valid-jsx-expression
          :name "expression"
          :mark-node t
          :template ("{" r "}"))
         (:description
          "{/* ... */}"
          :key ";"
          :shorthand valid-jsx-expression
          :name "comment"
          :mark-node t
          :template ("{/*" r "*/}"))
         (:description
          "{... ? ... : ...}"
          :key "?"
          :mark-node t
          :shorthand valid-jsx-expression
          :name "ternary"
          :template ("{" @ "null" >
                     n> " ? " @ > (choice* :name "consequence" :missing ("null" >) :rest (r>))
                     n> " : " @ > (choice* :name "alternative" :missing ("null" >) :rest (r>))
                     n> "}" >))
         (:description
          "...={ ... }"
          :key "=e"
          :mark-node nil
          :point-placement 'stay
          :nodes ("jsx_opening_element" "jsx_self_closing_element")
          :name "attr-expression"
          :template ("=" "{" @ "}"))
         (:description
          "...={ ... }"
          :key "=c"
          :mark-node nil
          :point-placement 'stay
          :nodes ("jsx_opening_element" "jsx_self_closing_element")
          :name "attr-choice"
          :template ("="
                     (choice* :name "Expression" :rest ("{" @ "}"))
                     (choice* :name "String" :rest ("\"" @ "\""))
                     (choice* :name "Object" :rest ("{{" @ "}}"))))
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

      (pretty-print-node-name-function #'combobulate-js-ts-pretty-print)
      (highlight-queries-default
       '(;; highlight the left-hand side of sequence expressions
         ;; ("the comma operator")
         ((arrow_function body: ((_ (sequence_expression left: (_) @hl.veggie)))))
         ;; highlight browser console object calls.
         ((call_expression function: (member_expression object: (identifier) @name @hl.serene
                                                        property: (property_identifier)
                                                        (:match "^console$" @name))))))
      (procedures-sequence
       '((:activation-nodes
          ((:nodes ("identifier") :position any :has-ancestor ("jsx_element")))
          :selector (:choose parent :match-query
                             (:query
                              (_ (jsx_opening_element (identifier) @match)
                                 (jsx_closing_element (identifier) @match))
                              :engine combobulate)))))
      (procedures-edit nil)
      (procedures-sexp
       '((:activation-nodes ((:nodes ("jsx_element"
                                      "regex"
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
                                      "function_declaration"
                                      "jsx_attribute"
                                      "jsx_fragment"
                                      "jsx_self_closing_element"))))))
      (procedures-defun
       '((:activation-nodes ((:nodes ("arrow_function" "function_declaration" "class_declaration" "method_definition"))))))

      (procedures-sibling
       `((:activation-nodes
          ((:nodes
            ((rule "pair"))
            :has-fields "key"
            :has-ancestor ((irule "pair"))))
          :selector (:choose
                     parent
                     :match-query
                     (:query (object (pair (_) @match)+) :engine combobulate)))
         (:activation-nodes
          ((:nodes
            ;; being javascript, you can put half the damn language
            ;; in the value part of an object pair
            ((rule-rx "expression"))
            :has-fields "value"
            :has-ancestor ((irule "pair"))))
          :selector (:choose
                     parent
                     :match-query
                     (:query (object (pair (_) (_) @match)+) :engine combobulate)))
         ;; for lists, arrays, objects, etc.
         (:activation-nodes
          ((:nodes
            ("import_specifier")
            :has-parent ("named_imports"))
           (:nodes
            (exclude
             ((rule "object") (rule "object_type") (rule "import_specifier")
              (rule "object_pattern") (rule "array") (rule "arguments") (rule "formal_parameters")
              (rule "expression") (rule "primary_expression") "arrow_function" (rule "tuple_type") (rule "union_type")
              (rule "intersection_type") (rule "type_arguments") (rule "array_pattern") (rule "type_arguments"))
             (irule "statement_block"))
            :has-parent
            ("object" "object_type" "import_specifier"
             "object_pattern" "array" "arguments" "formal_parameters"
             "expression" "primary_expression" "tuple_type" "union_type"
             "intersection_type" "type_arguments" "array_pattern" "type_arguments")))
          :selector (:match-children t))
         ;; for jsx
         (:activation-nodes
          ((:nodes
            ;; attributes can only appear in particular JSX elements
            ;; (namely opening and self-closing elements), so we need
            ;; a distinct rule for them.
            (rule "jsx_opening_element")
            :has-parent
            ("jsx_opening_element" "jsx_self_closing_element")))
          ;; but do exclude identifier as that'd match the tag name!
          :selector (:match-children (:match-rules (exclude (rule "jsx_opening_element") "identifier"))))

         ;; for general navigation
         (:activation-nodes
          ((:nodes
            ((exclude ((rule "jsx_element")) ("jsx_closing_element" "jsx_opening_element"))
             (rule "object")
             (rule "statement")
             (rule "declaration")
             (rule "statement_block")
             ;; for classes
             (rule "class_body")
             "program" "switch_case")
            :has-parent ("statement_block" "switch_body" "program"
                         "class_body"
                         (exclude ((rule "jsx_element")) ("jsx_closing_element" "jsx_opening_element")))))
          :selector (:match-children (:discard-rules ("comment" "jsx_closing_element" "jsx_opening_element"))))))

      (display-ignored-node-types '("jsx_opening_element" "jsx_closing_element"))
      (procedures-hierarchy
       `(;; general navigation into and out of blocks.
         (:activation-nodes
          ((:nodes
            ("arrow_function" "function_declaration" "class_declaration") :position at))
          :selector (:choose node :match-children
                             (:match-rules (rule "arrow_function" :body))))
         ;; this is here to general statements, like if, while,
         ;; etc. including one-armed if statements and those without
         ;; blocks.
         (:activation-nodes
          ((:nodes
            ("statement_block")
            :position at))
          :selector (:choose node :match-children t))
         ;; this handles the case where point is at the { ... } block
         ;; and it ensures it navigates into the first child.
         (:activation-nodes
          ((:nodes
            ((rule "statement"))
            :position at))
          ;; prefer statement_blocks to expressions
          :selector (:choose node :match-children (:match-rules ("statement_block"))))
         (:activation-nodes
          ((:nodes
            ((rule "statement"))
            :position at))
          :selector (:choose node :match-children (:match-rules (rule "expression"))))
         ;; allow seamless navigation between jsx elements
         (:activation-nodes
          ((:nodes ("jsx_fragment" "jsx_element" "jsx_expression")
                   ;; use `at' because it ensures that point is at the jsx
                   ;; element we wish to enter.
                   :position at))
          :selector
          (:choose node :match-children t))
         ;; inside a jsx opening node and we'll go 'down' into the
         ;; attributes.
         (:activation-nodes
          ((:nodes
            (("jsx_opening_element" "jsx_self_closing_element"))
            ;; use `in' because using at would conflict with general
            ;; jsx element navigation.
            :position in))
          :selector
          (:choose node :match-children (:discard-rules ("identifier"))))
         ;; inside a jsx attribute should take you to its value
         (:activation-nodes
          ((:nodes
            (("jsx_attribute"))
            ;; use `in' because using at would conflict with general
            ;; jsx element navigation.
            :position in))
          :selector
          (:choose node :match-children t))
         (:activation-nodes
          ((:nodes
            ((exclude
              (all)
              ;; disallow navigating to jsx element production rules from
              ;; this procedure, as it is handled below.
              (rule "jsx_element")
              "formal_parameters"))
            ;; Any parent but opening/closing elements as there's a
            ;; more specific rule below for that.
            :has-parent ((exclude (all) "jsx_opening_element" "jsx_self_closing_element"))))
          :selector (:choose node :match-children t))))
      (procedures-logical '((:activation-nodes ((:nodes (all))))))))

  (defconst combobulate-jsx-extra-defcustoms
    '((attribute-envelope-alist
       "Alist of envelopes to apply to certain JSX attributes when you press `='.

Where the car of the cell is the attribute identifier,
e.g. \"classname\", and the cdr is the type of envelope to
apply.

Two builtin ones include `attr-string', for string attributes;
and `attr-expression' for expression-based attributes."
       '(("className" . "attr-string")
         ("style" . "attr-expression-object")
         ("ref" . "attr-expression")
         ("choice" . "attr-choice"))
       :type '(alist :key-type string :value-type string))
      (attribute-envelope-default
       "Default envelope name to apply to a JSX attribute.

Only applied if `attribute-envelope-alist' does
not contain a valid JSX attribute alist entry.

If this value is `nil', then no envelope is applied.

The default value is `attr-choice' which is a good default for
most JSX attributes. It will insert a `choice' envelope unless
there's an explicit override. You can use `TAB' to cycle through
the options: a string attribute, an expression attribute or an
expression object attribute."
       "attr-choice"
       :type '(choice (const :tag "No envelope" nil)
                      (string :tag "Choice" "attr-choice")
                      (string :tag "String" "attr-string")
                      (string :tag "JSX Expression" "attr-expression")
                      (string :tag "JSX Expression + Object" "attr-expression-object")))
      (enable-auto-close-tag
       "Auto-close JSX tags when you type `>'."
       t
       :type 'boolean)
      (enable-attribute-envelopes
       "Pick a sensible value for a JSX attribute when you type `='.

This uses `attribute-envelope-alist' to
determine the attribute and the corresponding envelope to insert."
       t
       :type 'boolean))))

(define-combobulate-language
 :name javascript
 :language javascript
 ;; Oh, Javascript 🙄. And i'm sure this isn't even 20% of the
 ;; available modes.
 :major-modes (js-mode js2-mode rjsx-mode js3-mode javascript-mode
                       js-ts-mode js-jsx-mode js2-jsx-mode)
 :custom combobulate-jsx-definitions
 :extra-defcustoms combobulate-jsx-extra-defcustoms
 :setup-fn combobulate-js-ts-setup)

(define-combobulate-language
 :name typescript
 :language typescript
 :major-modes (typescript-mode typescript-ts-mode)
 :custom combobulate-jsx-definitions
 :extra-defcustoms combobulate-jsx-extra-defcustoms
 :setup-fn combobulate-js-ts-setup)

(define-combobulate-language
 :name tsx
 :language tsx
 :major-modes (tsx-ts-mode)
 :custom combobulate-jsx-definitions
 :extra-defcustoms combobulate-jsx-extra-defcustoms
 :setup-fn combobulate-js-ts-setup)

(defun combobulate-js-ts-setup (_)
  (when (combobulate-read enable-attribute-envelopes)
    (define-key (combobulate-read map) (kbd "=") #'combobulate-maybe-insert-attribute))
  (when (combobulate-read enable-auto-close-tag)
    (define-key (combobulate-read map) (kbd ">") #'combobulate-maybe-auto-close-tag))

  (setq combobulate-sgml-open-tag "jsx_opening_element")
  (setq combobulate-sgml-close-tag "jsx_closing_element")
  (setq combobulate-sgml-whole-tag "jsx_element")
  (setq combobulate-sgml-self-closing-tag "jsx_self_closing_element"))


(provide 'combobulate-js-ts)
;;; combobulate-javascript.el ends here
