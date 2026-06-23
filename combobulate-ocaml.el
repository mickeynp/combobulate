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

      (navigate-down-into-lists nil)
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
       '(

         (:activation-nodes
          ((:nodes ("tuple_expression") :position in))
          :selector (:choose node :match-siblings t))

         (:activation-nodes
          ((:nodes ("tuple_expression") :position at))
          :selector (:choose node :match-children t))

          ;; although the value_paths are siblings, here desired functionality will be to go to the sibling of their parent.

         (:activation-nodes
          ((:nodes ("comprehension_iterator") :position at
            :has-parent ("comprehension")))
          :selector (:choose parent :match-children t))

          ;; it's better to jump to the other bindings that navigate within the binding
          (:activation-nodes
          ((:nodes ("comprehension_binding") :position at
            :has-parent ("comprehension_iterator")))
          :selector (:choose parent :match-children t))

         (:activation-nodes
          ((:nodes ("let_binding") :position at :has-parent         ("value_definition"))
            (:nodes ("value_definition") :position at :has-parent ("let_expression"))
           (:nodes ("application_expression") :position at :has-parent ("sequence_expression"))
           (:nodes ("item_attribute") :position at)
           (:nodes ("infix_expression") :position at :has-parent ("comprehension")))
          :selector (:choose parent :match-children t)) ;; if match-siblings then we can do previous with navigate-prev if we could indicate which direction a rule should activate in we can solve this problem.

         (:activation-nodes
          ( (:nodes ("comprehension") :position at)
            (:nodes ("application_expression" "fun_expression") :position in))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ((rule "_type") (rule "_simple_type")) :position at
            :has-parent ("function_type")))
          :selector (:choose parent :match-children
                      (:match-rules ((rule "_type")
                                    (rule "_simple_type")))))

         (:activation-nodes
          ((:nodes ("type_variable") :position at
            :has-parent ("constructed_type")))
          :selector (:choose parent :match-children t))

         (:activation-nodes
          ((:nodes ("type_constructor_path") :position at
            :has-parent ("constructed_type")))
          :selector (:choose parent :match-siblings t))

        (:activation-nodes
          ((:nodes ("then_clause" "else_clause"
                    "value_path" "value_name" "constructor_path"
                    (rule "_simple_expression")) :position at
            :has-parent ("if_expression")))
          :selector (:choose parent :match-children
                      (:match-rules ((rule "_sequence_expression")
                                    (rule "_simple_expression")
                                    "then_clause"
                                    "else_clause"))))

        (:activation-nodes
          ((:nodes ("labeled_tuple_element_type" "labeled_tuple_element" "labeled_tuple_element_pattern" "match_expression") :position at))
          :selector (:choose parent :match-children t))

        (:activation-nodes
          ((:nodes ("external") :has-parent ((irule "external")) :position at))
          :selector (:choose node :match-siblings t))

        (:activation-nodes
          ((:nodes ("type_binding") :has-parent ("type_definition") :position at))
          :selector (:choose parent :match-children t))

         (:activation-nodes
          ((:nodes ( "constructor_pattern"))
           (:nodes ("type_constructor") :has-parent ("unboxed_type_constructor_path"))
          )
          :selector (:choose parent :match-siblings t))

         (:activation-nodes
          ((:nodes ("match_case") :position at))
          :selector (:choose node :match-siblings
                      (:match-rules ("match_case"))))

          (:activation-nodes
          ((:nodes ("mode" "mod" "jkind" "field_get_expression")))
          :selector (:choose node :match-siblings t))

         (:activation-nodes
          ((:nodes ("parameter") :position at
                   :has-parent ("let_binding")))
          :selector (:choose node :match-siblings t))

         (:activation-nodes
          ((:nodes ("variant_declaration"
                    "comprehension_binding"
                    "record_declaration"
                    "list_expression"
                    "cons_expression"
                    "field_get_expression"
                    "function_type"
                    "tuple_pattern"
                    "value_pattern"
                    "comprehension"
                    "tuple_expression"
                    "infix_expression"
                    "application_expression") :position in
            )
            (:nodes ((rule "jkind_mod"))))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ("value_definition"
                    "value_pattern"
                    "let_expression") :position at
                   :has-parent ("let_expression"))
            (:nodes ("mod" "mode"))
            )
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
                    "application_expression") :position at)
           (:nodes ((rule "signature")
                    (rule "structure"))
                   :has-ancestor ("module_definition")))
          :selector (:choose node :match-siblings t))


        (:activation-nodes
          ((:nodes (("type_binding")) :has-parent ("type_definition") :position in))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ("signature"
                    "structure"
                    "module_name"
                    "module_path"
                    "module_type_constraint")
                   :has-ancestor ("module_definition"
                                  "module_type_definition"
                                  "package_expression")
                   :position at)
           (:nodes ("comment"
                    "field_declaration"
                    "function_expression"
                    (rule "comprehension")
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
                    (rule "_structure_item")) :position at))
          :selector (:choose node :match-siblings (:discard-rules ("attribute" "ERROR"))))

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
                    (rule "comprehension")
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

       '(

        ;; DECISION: move to the body of a mtach directly
        ;; (:activation-nodes ((:nodes ("match_expression") :position at))
        ;; :selector (:choose node :match-children
        ;;           (:discard-rules ("value_name" "value_path" "tuple_expression"))))

        ;; DECISION: move down from an if directly to the else
        ;; (:activation-nodes ((:nodes ("if_expression") :position at))
        ;; :selector (:choose node :match-children
        ;;             (:match-rules ("then_clause" "else_clause"))))

        (:activation-nodes ((:nodes ("value_definition") :position at))
          :selector (:choose node :match-children
                    (:match-rules ("let_binding" "attribute"))))

        (:activation-nodes ((:nodes ("let_binding" 
                                     "fun_expression") 
                             :position at)
                             (:nodes ("infix_expression") :position at :has-parent ("comprehension")))
          :selector (:choose node :match-children t))

        (:activation-nodes ((:nodes ("let_expression") :position at))
          :selector (:choose node :match-children
                    (:match-rules ("value_definition"
                                    (rule "_sequence_expression")
                                    (rule "_simple_expression")))))

        (:activation-nodes ((:nodes ("application_expression") :position at))
          :selector (:choose node :match-children t))

        ;;  (:activation-nodes ((:nodes ("while_expression" "for_expression") :position at))
        ;;   :selector (:choose node :match-children
        ;;             t))

        ;; (:activation-nodes ((:nodes ("do_clause") :position at))
        ;;   :selector (:choose node :match-children
        ;;             t))

        ;; (:activation-nodes ((:nodes ("if_expression") :position at))
        ;;   :selector (:choose node :match-children
        ;;             t))

        ;; (:activation-nodes ((:nodes ("then_clause" "else_clause") :position at))
        ;;   :selector (:choose node :match-children
        ;;             t))

        (:activation-nodes
          ((:nodes ("match_expression" "try_expression" "function_expression") :position at))
          :selector (:choose node :match-children (:match-rules ("match_case"))))

        (:activation-nodes ((:nodes ("match_case") :position at))
          :selector (:choose node :match-children
                    t))

        (:activation-nodes
          ((:nodes ((rule "_pattern")
                    "constructor_path"
                    "value_path"
                    "value_pattern") :has-parent ("match_case") :position at))
          :selector (:choose parent :match-children
                    (:match-rules ((rule "_sequence_expression")
                                    (rule "_simple_expression")
                                    "refutation_case"
                                    "guard"))))

        (:activation-nodes ((:nodes ("function_type") :position at))
          :selector (:choose node :match-children
                    (:match-rules ((rule "_type")
                                    (rule "_simple_type")))))

        (:activation-nodes ((:nodes ("include_module") :position at))
          :selector (:choose node :match-children
                    (:match-rules ((rule "_module_expression")
                                    (rule "_simple_module_expression")))))

        (:activation-nodes ((:nodes ("attribute") :position at))
          :selector (:choose node :match-children
                    (:match-rules ("attribute_payload"))))                  
        
        (:activation-nodes
          ((:nodes ("field_get_expression"
                    "value_path" "typed_pattern"
                    "comprehension_iterator"
                    "parenthesized_operator"
                    "parenthesized_expression"
                    "parenthesized_pattern"
                    "tuple_pattern"
                    "application_expression"
                    "constructor_declaration"
                    "parameter" "at_mode_expr") :position at)
           (:nodes ((rule "polymorphic_variant_type"))))
          :selector (:choose node :match-children (:discard-rules ("|"))))

         (:activation-nodes
          ((:nodes ("object_expression"
                    (rule "class_definition")
                    (rule "object_expression")
                    (rule "class_binding")) :position at))
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

        (:activation-nodes ((:nodes ("signature") :position at))
        :selector (:choose node :match-children
                    (:match-rules ((rule "signature")))))

        (:activation-nodes ((:nodes ("structure") :position at))
        :selector (:choose node :match-children
                    (:match-rules ((rule "structure")))))

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
      (plausible-separators '(";" "," "|"))
      ;; Match the implementation side: don't let `combobulate-navigate-down'
      ;; fall back to `scan-lists' when no procedure matches.  The fallback
      ;; jumps into the first parenthesised expression it finds (e.g. into
      ;; `(string path * string)' of a val_specification's type), which is
      ;; never useful for a signature body.
      (navigate-down-into-lists nil)

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
       '(
         ;; Step between the bindings of a `type ... and ... and ...'
         ;; group inside a signature.  Mirror of the .ml side -- without
         ;; this rule, the catch-all below treats every type_binding's
         ;; contents as the activation and lands on the surrounding
         ;; type_definition instead of stepping between bindings.
         (:activation-nodes
          ((:nodes ("variant_declaration"
                    "record_declaration")))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ("type_binding")
                   :has-sibling ("type_binding")))
          :selector  (:choose node :match-siblings t))

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

         ;; From the `sig ... end' body of a module signature, descend
         ;; into its first item (value_specification, type_definition,
         ;; etc.).  `signature' is added to `combobulate-prefer-container-types'
         ;; so the `sig' keyword resolves to the signature container,
         ;; letting this rule fire at the keyword.  Without it the
         ;; cursor walks past the keyword to the first val_specification
         ;; (or wherever the anonymous-sibling logic lands), no rule
         ;; matches, and `combobulate-navigate-down' falls through to
         ;; the parenthesis-jump fallback -- landing inside the type
         ;; of the *last* val_specification rather than on the first
         ;; signature item.
         (:activation-nodes
          ((:nodes ("signature") :position at))
          :selector (:choose node :match-children
                             (:match-rules ((rule "_signature_item")))))

         ;; Descend from a constructor declaration into its payload
         ;; type (the `of T' part of `| Foo of T').  Mirrors the
         ;; equivalent .ml hierarchy rule; without it, cursor on a
         ;; constructor name in an .mli falls back to the
         ;; variant_declaration sibling rule and jumps to the *next*
         ;; constructor rather than descending into the payload of
         ;; the current one.  Must come before the type_binding rule
         ;; below so it matches first when cursor is on a
         ;; constructor_name inside a variant.
         (:activation-nodes
          ((:nodes ("constructor_declaration")))
          :selector (:choose node :match-children
                             (:discard-rules ("|"))))

         ;; Descend from a type binding into its variant declaration
         ;; (or record) so cursor on the type name (e.g. `path_item' in
         ;; `type 'cu path_item = | A | B') steps to the first
         ;; constructor.  The .ml side handles this via a separate
         ;; type_binding rule; in the interface we add an explicit
         ;; type_binding match because the catch-all rule below uses
         ;; `(rule "type_binding")' which expands to the type
         ;; binding's *contents*, not the binding itself, and so
         ;; never picks variant_declaration as a match target.
         (:activation-nodes
          ((:nodes ("type_binding") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("variant_declaration"
                                            "record_declaration"
                                            "constructed_type"
                                            "type_constructor_path"))))

         ;; Descend from `class type c = object ... end' through its
         ;; class_type_binding to the class_type_name and then the
         ;; class_body_type.  In the interface grammar the recursive
         ;; query used on the .ml side returns the class_type_binding
         ;; parent rather than its inner children, so step-by-step
         ;; rules are used instead.  `:position at' keeps each rule
         ;; from competing with the class_body_type descent below.
         (:activation-nodes ((:nodes ("class_type_definition") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("class_type_binding"))))
         (:activation-nodes ((:nodes ("class_type_binding") :position at))
          :selector (:choose node :match-children
                             (:match-rules ("class_type_name" "class_body_type"))))

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
;; 'ocaml-interface' for .mli files.
;; We register both as separate "languages" in Combobulate terms with their own
;; rule sets. Interface files (.mli) have a more restricted set of top-level
;; constructs (specifications rather than implementations).

(define-combobulate-language
 :name ocaml-interface
 :major-modes (caml-mode tuareg-interface-mode neocaml-interface-mode)
 :custom combobulate-ocaml-interface-definitions
 :setup-fn combobulate-ocaml-setup)
 
(define-combobulate-language
 :name ocaml
 :major-modes (caml-mode tuareg-mode neocaml-mode)
 :custom combobulate-ocaml-definitions
 :setup-fn combobulate-ocaml-setup)

(defun combobulate-ocaml-setup (_)
  "Setup function for OCaml mode with Combobulate.")

(provide 'combobulate-ocaml)
;;; combobulate-ocaml.el ends here
