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
          ((:nodes ("tuple_expression" "tuple_pattern") :position in))
          :selector (:choose node :match-siblings t))

         (:activation-nodes
          ((:nodes ("tuple_expression" "tuple_pattern") :position at))
          :selector (:choose node :match-children t))

          ;; although the value_paths are siblings, here desired functionality will be to go to the sibling of their parent.

          (:activation-nodes
           ((:nodes ("let_binding") :position at :has-parent         ("value_definition"))
             (:nodes ("value_definition") :position at :has-parent ("let_expression"))
            (:nodes ("application_expression") :position at :has-parent ("sequence_expression"))
            (:nodes ("item_attribute") :position at))
           :selector (:choose parent :match-children t))

          (:activation-nodes
           ((:nodes ("application_expression" "fun_expression") :position in))
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
          ((:nodes ("constructor_pattern")))
          :selector (:choose parent :match-siblings t))

         (:activation-nodes
          ((:nodes ("match_case") :position at))
          :selector (:choose node :match-siblings
                      (:match-rules ("match_case"))))

          (:activation-nodes
          ((:nodes ("field_get_expression")))
          :selector (:choose node :match-siblings t))

         (:activation-nodes
          ((:nodes ("parameter") :position at
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
                     "value_pattern"
                     "tuple_expression"
                     "infix_expression"
                     "application_expression") :position in))
           :selector (:choose node :match-children t))

         ;; Navigate between the bindings of a `type ... and ...' or
         ;; `let ... and ...' group (sibling type_binding / let_binding
         ;; nodes).  `:has-sibling' limits this to multi-binding groups;
         ;; a lone binding has no sibling and falls through to the
         ;; structure rules so top-level navigation still works.
         ;; Must come before the value_definition / let_expression rule
         ;; below, which would otherwise match the enclosing
         ;; value_definition first and step to the let_expression body
         ;; (skipping the `and' sibling).
         (:activation-nodes
          ((:nodes ("type_binding")
                   :has-sibling ("type_binding"))
           (:nodes ("let_binding")
                   :has-sibling ("let_binding")))
          :selector  (:choose node :match-siblings t))

          (:activation-nodes
           ((:nodes ("value_definition"
                     "value_pattern"
                     "let_expression") :position at
                    :has-parent ("let_expression"
                                 "let_open_expression")))
           :selector  (:choose parent :match-children t))

         ;; Navigate between record fields. Must come before the let
         ;; chain rule, otherwise let_open_expression (an ancestor)
         ;; matches first and intercepts navigation.
         (:activation-nodes
          ((:nodes ("field_expression")
                   :has-parent ("record_expression")))
          :selector  (:choose node :match-siblings t))

         ;; Navigate between the members of a class object
         ;; (`val'/`method') and the method specifications of a class
         ;; type, e.g. the methods of `class c = object ... end' or
         ;; `class type c = object ... end'.
         (:activation-nodes
          ((:nodes ("instance_variable_definition"
                    "method_definition"
                    "method_specification")))
          :selector  (:choose node :match-siblings t))

         ;; Navigate forward through let chains. From any
         ;; let_expression or let_open_expression, go to the body
         ;; child if it is another let or application.
         (:activation-nodes
          ((:nodes ("let_expression"
                    "let_open_expression")))
          :selector  (:choose node :match-children
                              (:match-rules ("let_expression"
                                             "let_open_expression"
                                             "application_expression"))))

         ;; Navigate forward through if/else if/else chains.
         ;; Like let chains, these are nested (else_clause contains
         ;; another if_expression), not flat siblings.
         ;; From if_expression or else_clause, navigate to the next
         ;; branch (else_clause or nested if_expression).
         (:activation-nodes
          ((:nodes ("if_expression" "else_clause")))
          :selector  (:choose node :match-children
                              (:match-rules ("else_clause"
                                             "if_expression"))))

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
                    "module_type_constraint") :position at
                   :has-ancestor ("module_definition"
                                  "module_type_definition"
                                  "package_expression"))
           ;; Navigation moves between top-level definitions and treats
           ;; a plain `attribute' (e.g. the `[@inline]' in
           ;; `let[@inline] f = ...') as a decoration: it is never a
           ;; navigation target and is skipped over between definitions.
           (:nodes ("comment"
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
                    (rule "structure")
                    (rule "signature")
                    (rule "_class_field_specification")
                    (rule "_sequence_expression")
                    (rule "_signature_item")
                    (rule "_structure_item")) :position at))
          :selector (:choose node :match-siblings (:discard-rules ("attribute"))))

         (:activation-nodes
          ((:nodes ((rule "compilation_unit"))))
          :selector (:choose node :match-children t))))

      (procedures-hierarchy

       ;; Class navigation: the class rules below navigate down
       ;; class -> name -> object body -> members.  navigate-up only
       ;; walks ancestors, so it does not revisit the name; this
       ;; down/up asymmetry is intentional.

       '(

        ;; DECISION: move to the body of a match directly
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
                             :position at))
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
        
        (:activation-nodes ((:nodes ("constructor_declaration") :has-parent ("variant_declaration") :position at))
          :selector (:choose node :match-children
                    (:match-rules ("constructed_type"))))

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
                    "parenthesized_operator"
                    "parenthesized_expression"
                    "parenthesized_pattern"
                    "tuple_pattern"
                    "application_expression"
                    "constructor_declaration"
                    "parameter") :position at)
           (:nodes ((rule "polymorphic_variant_type"))))
          :selector (:choose node :match-children (:discard-rules ("|"))))

         ;; From the class name, step to the object body.  Must come
         ;; before the object rule below, which would otherwise match
         ;; the enclosing `class_binding' and step to a value parameter.
         (:activation-nodes ((:nodes ("class_name")
                                     :has-parent ("class_binding")))
          :selector (:choose parent :match-children
                             (:match-rules ("object_expression"))))

         ;; Descend into an object body and its members.  `class_body_type'
         ;; (the body of a `class type') is included so down also reaches
         ;; its method_specification members.
         (:activation-nodes
          ((:nodes ("object_expression"
                    "class_body_type"
                    (rule "class_definition")
                    (rule "object_expression")
                    (rule "class_binding")) :position at))
          :selector (:choose node :match-children
                             (:discard-rules ("tag_specification"))))

         ;; Descend from a class / class type to its name then object
         ;; body, skipping type parameters, `virtual' and value
         ;; parameters.  The name and body are grandchildren of
         ;; class_definition / class_type_definition, so a recursive
         ;; query reaches them.  Must come after the object rule above so
         ;; that descending from inside the object (a `val'/`method')
         ;; stays local rather than jumping back to the name.
         (:activation-nodes ((:nodes ("class_definition")))
          :selector (:choose node :match-query
                             (:query ((class_binding
                                       [(class_name) (object_expression)] @match))
                              :engine treesitter)))
         (:activation-nodes ((:nodes ("class_type_definition")))
          :selector (:choose node :match-query
                             (:query ((class_type_binding
                                       [(class_type_name) (class_body_type)] @match))
                              :engine treesitter)))

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
                    "comment"
                    "field_declaration"
                    (rule "attribute_payload")
                    (rule "object_expression")
                    (rule "constructor_declaration")
                    (rule "class_binding")
                    (rule "type_binding")
                    (rule "signature")
                    (rule "_class_field_specification"))))
          :selector (:choose node :match-siblings (:discard-rules ("attribute"))))

         (:activation-nodes
          ((:nodes ((rule "compilation_unit"))))
          :selector (:choose node :match-children t)) ))

      (procedures-hierarchy
       '(

        (:activation-nodes
          ((:nodes ("instance_variable_specification") :position at))
          :selector (:choose node :match-children t))

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
;; 'ocaml-interface' for .mli files.
;; We register both as separate "languages" in Combobulate terms with their own
;; rule sets. Interface files (.mli) have a more restricted set of top-level
;; constructs (specifications rather than implementations).

(define-combobulate-language
 :name ocaml-interface
 :major-modes (caml-mode tuareg-mode neocaml-interface-mode)
 :custom combobulate-ocaml-interface-definitions
 :setup-fn combobulate-ocaml-setup)
 
(define-combobulate-language
 :name ocaml
 :major-modes (caml-mode tuareg-mode neocaml-mode)
 :custom combobulate-ocaml-definitions
 :setup-fn combobulate-ocaml-setup)

(defun combobulate-ocaml-setup (_)
  "Setup function for OCaml mode with Combobulate."
  (setq-local combobulate-navigate-down-into-lists nil
              ;; Make the opening keyword of a `signature' / `structure'
              ;; resolve to that container, so `C-M-n' on `sig' steps to
              ;; `struct' (siblings of module_binding) instead of
              ;; descending into the first signature item.
              combobulate-prefer-container-types '("signature" "structure")))

(provide 'combobulate-ocaml)
;;; combobulate-ocaml.el ends here
