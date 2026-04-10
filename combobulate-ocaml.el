;;; combobulate-ocaml.el --- ocaml support for combobulate -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Tim McGilchrist

;; Author: Tim McGilchrist <timmcgil@gmail.com>
;;         Pixie Dust <pizie@tarides.com>
;;         Xavier Van de Woestyne <xavier@tarides.com>
;; Keywords: convenience, tools, languages, ocaml

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-ocaml nil
  "Configuration switches for OCaml."
  :group 'combobulate
  :prefix "combobulate-ocaml-")

(defun combobulate-ocaml-pretty-print-node-name (node default-name)
  "Pretty print the NODE name (fallbacking on DEFAULT-NAME) for OCaml mode."
  (let ((name (treesit-node-text node t)))
    (if (string-empty-p name)
        default-name
      (combobulate-string-truncate
       (replace-regexp-in-string (rx (| (>= 2 " ") "\n")) " " name) 40))))

(eval-and-compile

  ;; Combobulate for implementation files (`ml').
  (defconst combobulate-ocaml-definitions
    '((context-nodes
       '("false" "true" "number" "class_name" "value_name"
         "module_name" "module_type_name" "field_name" "false" "true"))

      (envelope-indent-region-function #'indent-region)
      (envelope-list
       '((:description
          "let ... = ... in ..."
          :key "l"
          :name "let-binding"
          :template ("let " (p name "Name") " =" n> @ r> n> "in" n> @ n>))

         (:description
          "match ... with | ... -> ..."
          :key "m"
          :name "match-statement"
          :template ("match " (p expr "Expression") " with" n>
                     "| " (p pat "Pattern") " ->" n> @ r> n>
                     (choice* :missing nil
                              :rest ("| " (p pat2 "Next Pattern") " ->" n> @ n>)
                              :name "add-pattern")))

         (:description
          "if ... then ... else ..."
          :key "i"
          :name "if-statement"
          :template ("if " (p cond "Condition") " then" n> @ r> n>
                     (choice* :missing nil
                              :rest ("else" n> @ n>)
                              :name "else-branch")))

         (:description
          "try ... with | ... -> ..."
          :key "t"
          :name "try-with"
          :template ("try" n> @ r> n>
                     "with" n>
                     "| " (p exc "Exception") " ->" n> @ n>))

         (:description
          "module ... = struct ... end"
          :key "M"
          :name "module-struct"
          :template ("module " (p name "Module Name") " = struct" n>
                     @ r> n>
                     "end" > n>))

         (:description
          "begin ... end"
          :key "b"
          :name "begin-end"
          :template ("begin" n> @ r> n> "end" > n>))

         (:description
          "fun ... -> ..."
          :key "f"
          :name "fun-expression"
          :template ("fun " (p args "Arguments") " ->" n> @ r> n>))

         (:description
          "function | ... -> ..."
          :key "F"
          :name "function-expression"
          :template ("function" n>
                     "| " (p pat "Pattern") " ->" n> @ r> n>
                     (choice* :missing nil
                              :rest ("| " (p pat2 "Next Pattern") " ->" n> @ n>)
                              :name "add-pattern")))

         (:description
          "type ... = ..."
          :key "T"
          :name "type-definition"
          :template ("type " (p name "Type Name") " =" n> @ r> n>))

         (:description
          "module type ... = sig ... end"
          :key "S"
          :name "module-sig"
          :template ("module type " (p name "Signature Name") " = sig" n>
                     @ r> n>
                     "end" > n>))

         (:description
          "let open ... in ..."
          :key "o"
          :name "let-open"
          :template ("let open " (p mod "Module") " in" n> @ r> n>))

         (:description
          "for ... = ... to ... do ... done"
          :key "4"
          :name "for-loop"
          :template ("for " (p var "Variable") " = "
                     (p start "Start") " to " (p end "End") " do" n>
                     @ r> n>
                     "done" > n>))

         (:description
          "while ... do ... done"
          :key "w"
          :name "while-loop"
          :template ("while " (p cond "Condition") " do" n>
                     @ r> n>
                     "done" > n>))

         (:description
          "class ... = object ... end"
          :key "c"
          :name "class-definition"
          :template ("class " (p name "Class Name") " = object"
                     (choice* :missing nil
                              :rest (" (" (p self "self") ")")
                              :name "self-binding")
                     n> @ r> n>
                     "end" > n>))

         (:description
          "let rec ... = ... in ..."
          :key "r"
          :name "let-rec"
          :template ("let rec " (p name "Function Name") " =" n>
                     @ r> n>
                     "in" n> @ n>))

         (:description
          "struct ... end"
          :key "st"
          :name "anonymous-struct"
          :template ("struct" n> @ r> n> "end" > n>))

         (:description
          "type ... = | ... : ..."
          :key "G"
          :name "gadt-definition"
          :template ("type " (p params "Parameters") " " (p name "Type Name") " =" n>
                     "| " (p cons "Constructor") " : " (p ty "Constructor Type") n>
                     @ r> n>
                     (choice* :missing nil
                              :rest ("| " (p cons2 "Next Constructor")
                                     " : " (p ty2 "Next Type") n> @ n>)
                              :name "add-constructor")))))

      (pretty-print-node-name-function #'combobulate-ocaml-pretty-print-node-name)
      (plausible-separators '(";" "," "|" "struct" "sig" "end" "begin" "{" "}"))

      (display-ignored-node-types
       '("let" "module" "struct" "sig" "external"
         "val" "type" "class" "exception" "open" "include"))

      (procedures-logical '((:activation-nodes ((:nodes (all))))))

      (procedures-defun
       '((:activation-nodes
          ((:nodes ("type_definition" "exception_definition" "external"
                    "value_definition" "method_definition"
                    "instance_variable_definition" "module_definition"
                    "module_type_definition" "class_definition"))))))

      (procedures-sibling
       '((:activation-nodes
          ((:nodes ( "constructor_pattern" )))
          :selector (:choose parent :match-siblings t))

         (:activation-nodes
          ((:nodes ( "match_case" )))
          :selector (:choose node :match-siblings t))

         (:activation-nodes
          ((:nodes ("parameter")
                   :has-parent ("let_binding")))
          :selector (:choose node :match-siblings t))

         (:activation-nodes
          ((:nodes ("variant_declaration"
                    "record_declaration"
                    "list_expression"
                    "cons_expression"
                    "field_get_expression"
                    "function_type"
                    "tuple_pattern"
                    "value_pattern")))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ("value_definition"
                    "value_pattern"
                    "let_expression")
                   :has-parent ("let_expression")))
          :selector  (:choose parent :match-children t))

         (:activation-nodes
          ((:nodes ("type_variable"
                    "parameter"
                    "value_path"
                    "add_operator"
                    "mult_operator"
                    "pow_operator"
                    "rel_operator"
                    "concat_operator"
                    "or_operator"
                    "and_operator"
                    "assign_operator"
                    "infix_expression"
                    "type_constructor_path"
                    "field_declaration"
                    "tag_specification"
                    "match_case"
                    "field_expression"
                    "application_expression"))
           (:nodes ((rule "signature")
                    (rule "structure"))
                   :has-ancestor ("module_definition")))
          :selector (:choose node :match-siblings t))

         (:activation-nodes
          ((:nodes ("signature"
                    "structure"
                    "module_name"
                    "module_path"
                    "module_type_constraint")
                   :has-ancestor ("module_definition"
                                  "module_type_definition"
                                  "package_expression"))
           (:nodes ("attribute"
                    "comment"
                    "field_declaration"
                    "function_expression"
                    (rule "function_type")
                    (rule "attribute_payload")
                    (rule "record_expression")
                    (rule "object_expression")
                    (rule "constructor_declaration")
                    (rule "class_binding")
                    (rule "class_application")
                    (rule "type_binding")
                    (rule "method_definition")
                    (rule "structure")
                    (rule "signature")
                    (irule "signature")
                    (irule "structure")
                    (rule "_class_field_specification")
                    (rule "_sequence_expression")
                    (rule "_signature_item")
                    (rule "_structure_item"))))
          :selector (:choose node :match-siblings t))

         (:activation-nodes
          ((:nodes ((rule "compilation_unit"))))
          :selector (:choose node :match-children t))))

      (procedures-hierarchy

       ;; NOTE: Known limitation regarding navigation within class
       ;; hierarchies.
       ;; Navigation will go through:
       ;;   class
       ;;    → class_definition
       ;;    → class_binding
       ;;    → class_name
       ;;    → parameter
       ;;    → parameter
       ;;    → object_expression
       ;; But we want:
       ;;   class
       ;;    → class_name
       ;;    → object
       ;;    → instance_variable_definition
       ;; This appears to be either:
       ;;   1. A limitation in how combobulate processes selector
       ;;      rules for certain grammars
       ;;   2. An issue specific to the OCaml tree-sitter grammar structure
       ;;   3. A bug in the combobulate procedure matching logic

       ;; Pretty printing rules for `class_binding' gives us:
       ;; - :*unnamed*: ("abstract_type" "item_attribute"
       ;;                "parameter" "class_function_type" "type_variable")
       ;; - :body: ("class_function" "let_open_class_expression"
       ;;            "let_class_expression" "class_application")
       ;; - :name ("class_name")

       ;; the object expression does not appear in these rules which is probably
       ;; part of the problem.

       '((:activation-nodes
          ((:nodes ("field_get_expression"
                    "value_path"
                    "paranthesized_operator"
                    "application_expression"
                    "constructor_declaration"
                    "parameter"))
           (:nodes ((rule "polymorphic_variant_type"))))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ("object_expression"
                    (rule "class_definition")
                    (rule "object_expression")
                    (rule "class_binding"))))
          :selector (:choose node :match-children
                             (:discard-rules ("tag_specification"))))

         ;; Specific rules for converting from a constructor
         ;; (of a Sum type) to its type
         (:activation-nodes
          ((:nodes ("constructor_name")
                   :has-parent ("constructor_declaration")))
          :selector (:choose parent :match-children
                             (:nodes ("type_constructor_path"))))

         ;; Specific rules for converting from a constructor
         ;; (of a Sum type) to its type
         (:activation-nodes
          ((:nodes ("type_constructor_path")))
          :selector (:choose node :match-children
                             (:nodes ("type_constructor"))))

         (:activation-nodes
          ((:nodes ("signature"
                    "structure"
                    "module_name"
                    "module_path")
                   :has-ancestor ("module_definition"
                                  "module_type_definition"
                                  "package_expression"))

           ;; We remove class_application, class_binding and object_expression
           ;; to avoid conflict (See the rules above)
           (:nodes ((rule "module_definition")
                    (rule "record_declaration")
                    (rule "attribute_payload")
                    (rule "function_type")
                    (irule "function_type")
                    (irule "set_expression")
                    (irule "infix_expression")
                    (rule "constructor_declaration")
                    (rule "type_binding")
                    (rule "method_definition")
                    (irule "value_path")
                    (irule "signature")
                    (irule "structure")
                    (rule "_signature_item")
                    (rule "_structure_item"))))
          :selector (:choose node :match-children t))

         ;; This should be equivalent to listing everything in "compilation_unit"
         (:activation-nodes
          ((:nodes (rule "compilation_unit")))
          :selector (:choose node :match-children t))))))


  ;; Combobulate for interface files (`mli').
  ;; Asubset of constructs compared to implementation files
  (defconst combobulate-ocaml-interface-definitions
    '((context-nodes
       '("false" "true" "number" "class_name" "value_name"
         "module_name" "module_type_name" "field_name"
         "module" "sig" "end" "val" "type" "class" "exception"
         "open" "external" ":" ";" "," "|" "->" "=" "(" ")" "[" "]" "{" "}"))

      (envelope-indent-region-function #'indent-region)
      (pretty-print-node-name-function #'combobulate-ocaml-pretty-print-node-name)
      (plausible-separators '(";" ",", "|"))

      ;; Interface files only have specifications, not definitions
      ;; This is why declaration and type_specs are discared.
      (procedures-defun
       '((:activation-nodes
          ((:nodes ("type_definition"
                    "exception_definition"
                    "value_specification"
                    "module_definition"
                    "module_type_definition"
                    "class_definition"
                    "class_type_definition"
                    "include_module"
                    "include_module_type"
                    "open_module"))))))

      (procedures-logical '((:activation-nodes ((:nodes (all))))))

      (procedures-sibling
       '((:activation-nodes
          ((:nodes ("variant_declaration"
                    "record_declaration")))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes (
                    ;; Top-level interface items - using rule to get
                    ;; all _signature_item types
                    (rule "_signature_item")

                    ;; Regular nodes
                    "attribute"
                    "comment"
                    "field_declaration"
                    (rule "attribute_payload")
                    (rule "object_expression")
                    (rule "constructor_declaration")
                    (rule "class_binding")
                    (rule "type_binding")
                    (rule "signature")
                    (irule "signature")
                    (rule "_class_field_specification"))))
          :selector (:choose node :match-siblings t))

         (:activation-nodes
          ((:nodes ((rule "compilation_unit"))))
          :selector (:choose node :match-children t)) ))

      (procedures-hierarchy
       '(

         ;; From module_name, navigate up to parent then to signature
         (:activation-nodes
          ((:nodes ("module_name")))
          :selector (:choose parent :match-children
                             (:match-rules ("signature"))))

         ;; From sig keyword, navigate to sibling signature items
         ;; (type_definition, value_specification, etc.)
         (:activation-nodes
          ((:nodes ("sig")))
          :selector (:choose node :match-siblings
                             (:match-rules ((rule "_signature_item")))))

         ;; From method keyword, navigate to parent then to method_name
         (:activation-nodes
          ((:nodes ("method")))
          :selector (:choose parent :match-children
                             (:match-rules ("method_name"))))

         ;; For signature and other structural nodes, match their children
         (:activation-nodes
          ((:nodes ((rule "attribute_payload")
                    (rule "object_expression")
                    (rule "constructor_declaration")
                    (rule "class_binding")
                    "class_body_type"
                    "method_specification"
                    (rule "type_binding")
                    (rule "signature")
                    (irule "signature")
                    (rule "_signature_item"))))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes (rule "compilation_unit")))
          :selector (:choose node :match-children t)))))))

;; NOTE: OCaml has two tree-sitter grammars: 'ocaml' for .ml files and
;; 'ocaml_interface' for .mli files.
;; We register both as separate "languages" in Combobulate terms with their own
;; rule sets. Interface files (.mli) have a more restricted set of top-level
;; constructs (specifications rather than implementations).
;; The `:language' parameter matches what tree-sitter uses,
;; while the :name is used for Emacs Lisp symbol names.

(define-combobulate-language
 :name ocaml
 :language ocaml
 :major-modes (caml-mode tuareg-mode)
 :custom combobulate-ocaml-definitions
 :setup-fn combobulate-ocaml-setup)

(define-combobulate-language
 :name ocaml-interface
 :language ocaml_interface
 :major-modes (caml-mode tuareg-mode)
 :custom combobulate-ocaml-interface-definitions
 :setup-fn combobulate-ocaml-setup)

(defun combobulate-ocaml-setup (_)
  "Setup function for OCaml mode with Combobulate."
  (setq-local combobulate-navigate-down-into-lists nil))

(provide 'combobulate-ocaml)
;;; combobulate-ocaml.el ends here
