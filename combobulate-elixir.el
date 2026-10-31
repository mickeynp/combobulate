;;; combobulate-elixir.el --- elixir support for combobulate  -*- lexical-binding: t; -*-

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

;; Supports the tree-sitter-elixir grammar.
;;
;; In that grammar `def', `case', `if' and `Enum.map(...)' are all
;; `call' nodes.  A call with a `do' block holds its statements or
;; clauses in a `do_block', and a call without one holds its
;; arguments in `arguments'.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-elixir nil
  "Configuration switches for Elixir"
  :group 'combobulate
  :prefix "combobulate-elixir-")

(defun combobulate-elixir-pretty-print-node-name (node _default-name)
  "Pretty printer for Elixir nodes"
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("call"
       (let ((target (combobulate-node-child-by-field node "target"))
             (arguments (combobulate-node-child node 1)))
         (if (and arguments (equal (combobulate-node-type arguments) "arguments"))
             (concat (combobulate-node-text target) " "
                     (combobulate-node-text (combobulate-node-child arguments 0)))
           (combobulate-node-text target))))
      ("stab_clause"
       (concat (combobulate-node-text (combobulate-node-child-by-field node "left")) " ->"))
      (_ (car (split-string (combobulate-node-text node) "\n")))))
   40))

(eval-and-compile
  (defconst combobulate-elixir--blocks
    '("source" "do_block" "else_block" "rescue_block" "catch_block" "after_block"
      "body" "block" "anonymous_function")
    "Node types whose children are statements or clauses.")

  (defconst combobulate-elixir--collections
    '("arguments" "list" "tuple" "map_content" "keywords" "bitstring")
    "Node types whose children are comma-separated elements.")

  (defvar combobulate-elixir-definitions
    `((context-nodes
       '("identifier" "alias" "atom" "keyword"))
      (plausible-separators '("," "\n"))
      (procedure-discard-rules '("comment"))
      (pretty-print-node-name-function #'combobulate-elixir-pretty-print-node-name)
      (procedures-defun
       '((:activation-nodes ((:nodes ("call" "anonymous_function"))))))
      (procedures-sibling
       '(;; The statement or clause at point.
         (:activation-nodes
          ((:nodes ((all)) :position at :has-parent ,combobulate-elixir--blocks))
          :selector (:choose parent :match-children t))
         ;; The elements of an argument list or a collection.
         (:activation-nodes
          ((:nodes ((all)) :has-parent ,combobulate-elixir--collections))
          :selector (:choose parent :match-children t))
         ;; The stages of a pipeline.
         (:activation-nodes
          ((:nodes ("binary_operator") :has-parent ,combobulate-elixir--blocks))
          :selector (:choose node
                             :match-query (:query ((binary_operator operator: "|>" right: (_) @match))
                                                  :engine treesitter)))
         ;; The statement or clause point is in.
         (:activation-nodes
          ((:nodes ((all)) :has-parent ,combobulate-elixir--blocks))
          :selector (:choose parent :match-children t))))
      (procedures-hierarchy
       '(;; From a call with a `do' block, its statements or clauses.
         (:activation-nodes
          ((:nodes ("call") :position at))
          :selector (:choose node
                             :match-query (:query (call (do_block (_)+ @match))
                                                  :engine combobulate)))
         ;; From any other call, its arguments.
         (:activation-nodes
          ((:nodes ("call") :position at))
          :selector (:choose node
                             :match-query (:query (call (arguments (_)+ @match))
                                                  :engine combobulate)))
         ;; From a clause, its body.
         (:activation-nodes
          ((:nodes ("stab_clause") :position at))
          :selector (:choose node
                             :match-query (:query (stab_clause (body (_)+ @match))
                                                  :engine combobulate)))
         ;; From the head of a pipeline, its stages.
         (:activation-nodes
          ((:nodes ("binary_operator") :position at))
          :selector (:choose node
                             :match-query (:query ((binary_operator operator: "|>" right: (_) @match))
                                                  :engine treesitter)))
         (:activation-nodes
          ((:nodes ((all)) :position at))
          :selector (:choose node :match-children t))))
      ;; The keywords of the construct around point, such as `do' and `end'.
      (procedures-sequence
       '((:activation-nodes
          ((:nodes ("call") :position any))
          :selector (:choose node
                             :match-query (:query (call (do_block "do" @match "end" @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("anonymous_function") :position any))
          :selector (:choose node
                             :match-query (:query (anonymous_function "fn" @match "end" @match)
                                                  :engine combobulate))))))))

(define-combobulate-language
 :name elixir
 :major-modes (elixir-ts-mode)
 :custom combobulate-elixir-definitions
 :setup-fn combobulate-elixir-setup)

(defun combobulate-elixir-setup (_))

(provide 'combobulate-elixir)
;;; combobulate-elixir.el ends here
