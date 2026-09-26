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
;; `call' nodes, so defun navigation cannot be expressed as node
;; types.  Instead, the defun commands are remapped to the
;; `treesit-*-defun' commands, which use the major mode's
;; `treesit-defun-type-regexp' predicate.

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

(defun combobulate-elixir-pretty-print-node-name (node default-name)
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
      (_ default-name)))
   40))

(eval-and-compile
  (defvar combobulate-elixir-definitions
    '((context-nodes
       '("identifier" "alias" "atom" "keyword"))
      (plausible-separators '("," "\n"))
      (pretty-print-node-name-function #'combobulate-elixir-pretty-print-node-name)
      (procedures-sibling
       '(;; A clause head such as `n < 0' sits in an `arguments' node, so
         ;; clauses must be matched before the generic rule below.
         (:activation-nodes
          ((:nodes ("stab_clause")
                   :position at
                   :has-parent ("do_block" "else_block" "rescue_block" "catch_block"
                                "after_block" "anonymous_function")))
          :selector (:choose parent :match-children t))
         (:activation-nodes
          ((:nodes
            ((all))
            :has-parent ("source" "do_block" "else_block" "rescue_block" "catch_block"
                         "after_block" "body" "block" "anonymous_function"
                         "arguments" "list" "tuple" "map_content" "keywords" "bitstring")))
          :selector (:choose parent :match-children t))))
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ("call") :position at))
          :selector (:choose node
                             :match-query (:query (call (do_block (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("stab_clause") :position at))
          :selector (:choose node
                             :match-children (:match-rules ("body"))))
         (:activation-nodes
          ((:nodes ("call") :position at))
          :selector (:choose node
                             :match-query (:query (call (arguments (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ((exclude (all) "do_block" "body" "arguments" "keywords" "map_content"))
                   :position at))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name elixir
 :major-modes (elixir-ts-mode)
 :custom combobulate-elixir-definitions
 :setup-fn combobulate-elixir-setup)

(defun combobulate-elixir-setup (_)
  (let ((map (combobulate-read map)))
    (define-key map [remap combobulate-navigate-beginning-of-defun] #'treesit-beginning-of-defun)
    (define-key map [remap combobulate-navigate-end-of-defun] #'treesit-end-of-defun)
    (define-key map [remap combobulate-mark-defun] #'mark-defun)))

(provide 'combobulate-elixir)
;;; combobulate-elixir.el ends here
