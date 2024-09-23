;;; lang/rust/combobulate-rust.el -*- lexical-binding: t; -*-

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-interface)
(require 'combobulate-rules)

(defun combobulate-rust--get-function-name (node)
  (concat "fn "
          (car (combobulate-query-node-text
                '((function_item (identifier) @name))
                node t))))

(defun combobulate-rust-pretty-print-node-name (node default-name)
  "Pretty printer for Rust nodes"
  (pcase (combobulate-node-type node)
    ("function_item" (combobulate-rust--get-function-name node))
    (_ default-name)))

(eval-and-compile
  (defvar combobulate-rust-definitions
    '((envelope-procedure-shorthand-alist
       '())
      (envelope-list
       '((:description
           "( ... )"
           :key "("
           :extra-key "M-("
           :mark-node t
           :nodes ,(append
                    (combobulate-production-rules-get "primary_expression")
                    (combobulate-production-rules-get "expression"))
           :name "wrap-parentheses"
           :template (@ "(" r ")"))))
      (context-nodes
       '("identifier" "type_identifier" "primitive_type" "field_identifier" "string_literal" "char_literal" "raw_string_literal"
          "integer_literal" "float_literal"))
      (indent-after-edit t)
      (envelope-indent-region-function #'indent-region)
      (procedures-edit nil)
      (pretty-print-node-name-function #'combobulate-rust-pretty-print-node-name)
      (procedures-sequence nil)
      (procedures-sexp nil)
      (plausible-separators '(";" ","))
      (procedures-defun
       '((:activation-nodes ((:nodes ("struct_item"
                                      "enum_item"
                                      "union_item"
                                      "function_item"
                                      "const_item"
                                      "static_item"
                                      "trait_item"
                                      "mod_item"
                                      "macro_definition"
                                      "inner_attribute_item"
                                      "function_signature_item"
                                      "foreign_mod_item"
                                      "extern_crate_declaration"
                                      "associated_type"
                                      "type_item"
                                      ;; "impl_item" ?
                                      ))))))
      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))
      (procedures-sibling
       `((:activation-nodes
          ((:nodes ((rule "field_declaration_list"))
                   :has-parent ("field_declaration_list"))
           (:nodes ((rule "field_initializer_list"))
                   :has-parent ("field_initializer_list"))
           ;; FIXME: attributes are siblings instead of being attached to the parameter, which makes dragging them annoying
           (:nodes ("parameter")
                   :has-parent ("parameters"))
           (:nodes ((rule "block"))
                   :has-parent "block")
           (:nodes ((rule "source_file"))
                   :has-parent "source_file")
           (:nodes ((rule "declaration_list"))
                   :has-parent "declaration_list")
           (:nodes ("match_arm")
                   :has-parent "match_block"))
          :selector (:choose parent :match-children t))))
      (procedures-hierarchy
       `()))))

(define-combobulate-language
 :name rust
 :language rust
 :major-modes (rust-mode rust-ts-mode)
 :custom combobulate-rust-definitions
 :setup-fn combobulate-rust-setup)

(defun combobulate-rust-setup (_))

(provide 'combobulate-rust)
;;; combobulate-rust.el ends here
