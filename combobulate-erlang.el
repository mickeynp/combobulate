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
       '((:activation-nodes
          ((:nodes ,combobulate-erlang--clause-types
                   :position at
                   :has-parent ("case_expr" "receive_expr" "if_expr" "try_expr" "anonymous_fun")))
          :selector (:choose parent
                             :match-children (:match-rules ,combobulate-erlang--clause-types)))
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
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name erlang
 :major-modes (erlang-ts-mode erlang-mode)
 :custom combobulate-erlang-definitions
 :setup-fn combobulate-erlang-setup)

(defconst combobulate-erlang--keyword-types
  '("case" "of" "receive" "if" "try" "catch" "after" "begin" "fun" "maybe" "else" "end")
  "Keyword tokens that delimit an Erlang construct.")

(defun combobulate-erlang--keywords (node)
  "Return the start positions of the keywords that delimit NODE.

The `after' of `try' and `receive' sits one level down, in its own
`try_after' or `receive_after' node."
  (when (member (treesit-node-type node)
                '("case_expr" "receive_expr" "if_expr" "try_expr" "block_expr"
                  "anonymous_fun" "maybe_expr"))
    (mapcan (lambda (child)
              (cond
               ((member (treesit-node-type child) '("try_after" "receive_after"))
                (list (treesit-node-start child)))
               ((and (not (treesit-node-check child 'named))
                     (member (treesit-node-type child) combobulate-erlang--keyword-types))
                (list (treesit-node-start child)))))
            (treesit-node-children node))))

(defun combobulate-erlang--sequence-target (direction)
  "Return the next keyword position in DIRECTION among the constructs around point."
  (let ((node (treesit-node-at (point) 'erlang))
        (target))
    (while (and node (not target))
      (let ((positions (combobulate-erlang--keywords node)))
        (setq target (if (eq direction 'next)
                         (seq-find (lambda (pos) (> pos (point))) positions)
                       (car (last (seq-filter (lambda (pos) (< pos (point))) positions))))))
      (setq node (treesit-node-parent node)))
    target))

(defun combobulate-erlang-navigate-sequence-next (&optional arg)
  "Move to the next keyword of the construct at point ARG times.

From `case' this visits `of', then `end'; from `try' also `catch'
and `after'."
  (interactive "^p")
  (dotimes (_ (or arg 1))
    (when-let* ((target (combobulate-erlang--sequence-target 'next)))
      (goto-char target))))

(defun combobulate-erlang-navigate-sequence-previous (&optional arg)
  "Move to the previous keyword of the construct at point ARG times."
  (interactive "^p")
  (dotimes (_ (or arg 1))
    (when-let* ((target (combobulate-erlang--sequence-target 'previous)))
      (goto-char target))))

(defun combobulate-erlang-setup (_)
  (let ((map (combobulate-read map)))
    (define-key map [remap combobulate-navigate-sequence-next]
                #'combobulate-erlang-navigate-sequence-next)
    (define-key map [remap combobulate-navigate-sequence-previous]
                #'combobulate-erlang-navigate-sequence-previous)))

(provide 'combobulate-erlang)
;;; combobulate-erlang.el ends here
