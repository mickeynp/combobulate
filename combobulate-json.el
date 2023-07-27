;;; combobulate-json.el --- JSON mode support for Combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
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

;;

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-interface)
(require 'combobulate-rules)

(defun combobulate-json-pretty-print-node-name (node default-name)
  "Pretty printer for JS and JSX nodes"
  (pcase (combobulate-node-type node)
    ("pair" (combobulate-node-text (combobulate-node-child-by-field node "key")))
    ("string" (combobulate-string-truncate
               (concat "\"" (combobulate-node-text node) "\"")
               40))
    (_ default-name)))

(defun combobulate-json-setup (_)
  (setq combobulate-navigation-context-nodes
        '("string" "string_content" "number" "null" "true" "false"))

  (setq combobulate-manipulation-envelopes nil)

  (setq combobulate-pretty-print-node-name-function #'combobulate-json-pretty-print-node-name)
  (setq combobulate-manipulation-trim-whitespace 'backward)
  (setq combobulate-manipulation-trim-empty-lines t)
  (setq combobulate-highlight-queries-default
        '(;; highlight pseudo "comments" that are often designated "//"
          ((pair key: (string (string_content) @hl.comment (:match "^//$" @hl.comment))) @hl.comment)))
  (setq combobulate-manipulation-edit-procedures
        `(;; editing an element's opening/closing tag
          (:activation-nodes
           ((:node "object" :position at-or-in))
           :match-query (object (pair)+ @match))))
  (setq combobulate-navigation-sibling-skip-prefix t)
  (setq combobulate-navigation-sexp-nodes '("pair"))
  (setq combobulate-manipulation-splicing-procedures
        `((:activation-nodes
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

  (setq combobulate-navigation-drag-parent-nodes '("object" "array"))
  (setq combobulate-navigation-defun-nodes '("document"))

  (setq combobulate-navigation-sibling-procedures
        `(
          ;; help clone work with array elements
          (:activation-nodes
           ((:node
             ,(combobulate-production-rules-get "array")
             :position at
             :find-immediate-parent ,(combobulate-production-rules-get "array")))
           :match-children t)
          ;; help clone find pairs
          (:activation-nodes
           ((:node ("pair") :position at :find-immediate-parent ("object")))
           :match-children t)
          ;; for general navigation
          (:activation-nodes
           ((:node
             ("named_imports" "object" "formal_parameters" "array" "object_type" "arguments")
             :position at-or-in))
           :remove-types ("comment")
           :match-query ((_) (_)+ @match))))

  (setq combobulate-navigation-parent-child-nodes `("document" "object" "array" "pair"))

  (setq combobulate-navigation-default-nodes `("document" "object" "array" "pair"))
  (setq combobulate-navigation-logical-nodes (seq-uniq (flatten-tree combobulate-rules-json-inverted))))
(provide 'combobulate-json)
;;; combobulate-json.el ends here
