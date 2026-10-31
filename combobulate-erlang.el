;;; combobulate-erlang.el --- erlang support for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Victor Rodrigues

;; Author: Victor Rodrigues
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

;; Supports the WhatsApp tree-sitter-erlang grammar, as used by
;; `erlang-ts-mode'.
;;
;; In that grammar each clause of a function is its own `fun_decl'
;; form, so the clauses of `area/1' are siblings at the top level.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-erlang nil
  "Configuration switches for Erlang"
  :group 'combobulate
  :prefix "combobulate-erlang-")

(defun combobulate-erlang-pretty-print-node-name (node default-name)
  "Pretty printer for Erlang nodes"
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("fun_decl"
       (let* ((clause (combobulate-node-child-by-field node "clause"))
              (name (combobulate-node-text (combobulate-node-child-by-field clause "name")))
              (args (combobulate-node-child-by-field clause "args")))
         (format "%s/%d" name (treesit-node-child-count args t))))
      (_ default-name)))
   40))

(eval-and-compile
  (defconst combobulate-erlang--clause-types
    '("cr_clause" "if_clause" "fun_clause" "catch_clause" "receive_after" "try_after")
    "Node types that are clauses of a `case', `receive', `if', `fun' or `try'.")

  (defconst combobulate-erlang--wrappers
    '("clause_body" "expr_args" "var_args" "macro_call_args" "macro_expr"
      "guard" "guard_clause" "lc_exprs")
    "Node types that only group other nodes, so up skips them.")

  (defvar combobulate-erlang-definitions
    `((context-nodes
       '("atom" "var" "integer" "float" "string" "char"))
      (pretty-print-node-name-function #'combobulate-erlang-pretty-print-node-name)
      (procedures-defun
       '((:activation-nodes ((:nodes ("fun_decl"))))))
      (procedures-sibling
       '(;; `after' must stay last, so it is not a sibling of the clauses.
         (:activation-nodes
          ((:nodes ("cr_clause" "if_clause" "fun_clause" "catch_clause")
                   :position at
                   :has-parent ("case_expr" "receive_expr" "if_expr" "try_expr" "anonymous_fun")))
          :selector (:choose parent
                             :match-children (:match-rules ("cr_clause" "if_clause" "fun_clause" "catch_clause"))))
         (:activation-nodes
          ((:nodes ("record_field")
                   :position at
                   :has-parent ("record_expr" "record_update_expr" "record_decl")))
          :selector (:choose parent :match-children (:match-rules ("record_field"))))
         (:activation-nodes
          ((:nodes ("map_field")
                   :position at
                   :has-parent ("map_expr" "map_expr_update")))
          :selector (:choose parent :match-children (:match-rules ("map_field"))))
         (:activation-nodes
          ((:nodes ((exclude (all) ,@combobulate-erlang--clause-types))
                   :position at
                   :has-parent ("try_expr")))
          :selector (:choose parent
                             :match-children (:discard-rules ,combobulate-erlang--clause-types)))
         (:activation-nodes
          ((:nodes ((all))
                   :has-parent ("source_file" "clause_body" "block_expr" "try_after"
                                "expr_args" "var_args" "macro_call_args"
                                "list" "tuple" "binary" "guard" "guard_clause" "lc_exprs"
                                "export_attribute" "export_type_attribute")))
          :selector (:choose parent :match-children t))))
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ("fun_decl") :position at))
          :selector (:choose node
                             :match-query
                             (:query (fun_decl (function_clause (clause_body (_)+ @match)))
                                     :engine combobulate)))
         (:activation-nodes
          ((:nodes ("function_clause" "cr_clause" "if_clause" "fun_clause"
                    "catch_clause" "receive_after")
                   :position at))
          :selector (:choose node
                             :match-query (:query (_ (clause_body (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("case_expr" "receive_expr" "if_expr" "anonymous_fun") :position at))
          :selector (:choose node
                             :match-children (:match-rules ,combobulate-erlang--clause-types)))
         (:activation-nodes
          ((:nodes ("call") :position at))
          :selector (:choose node
                             :match-query (:query (call (expr_args (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ((exclude (all) ,@combobulate-erlang--wrappers)) :position at))
          :selector (:choose node :match-children t))))
      (procedures-sequence
       '((:activation-nodes
          ((:nodes ("case_expr") :position any))
          :selector (:choose node
                             :match-query (:query (case_expr "case" @match "of" @match "end" @match)
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("receive_expr") :position any))
          :selector (:choose node
                             :match-query (:query (receive_expr "receive" @match "end" @match)
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("if_expr") :position any))
          :selector (:choose node
                             :match-query (:query (if_expr "if" @match "end" @match)
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("block_expr") :position any))
          :selector (:choose node
                             :match-query (:query (block_expr "begin" @match "end" @match)
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("anonymous_fun") :position any))
          :selector (:choose node
                             :match-query (:query (anonymous_fun "fun" @match "end" @match)
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("try_expr") :position any))
          :selector (:choose node
                             :match-query (:query (try_expr "try" @match "end" @match)
                                                  :engine combobulate))))))))

(define-combobulate-language
 :name erlang
 :major-modes (erlang-ts-mode erlang-mode)
 :custom combobulate-erlang-definitions
 :setup-fn combobulate-erlang-setup)

(defun combobulate-erlang-setup (_))

(provide 'combobulate-erlang)
;;; combobulate-erlang.el ends here
