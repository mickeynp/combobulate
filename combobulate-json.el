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
(require 'combobulate-setup)

(defun combobulate-json-pretty-print-node-name (node default-name)
  "Pretty print the node name for JSON mode."
  (pcase (combobulate-node-type node)
    ("pair" (combobulate-node-text (combobulate-node-child-by-field node "key")))
    ("string" (combobulate-string-truncate
               (concat (combobulate-node-text node))
               40))
    (_ default-name)))

(eval-and-compile
  (defvar combobulate-json-definitions
    '((context-nodes
       '("string" "string_content" "number" "null" "true" "false"))
      (envelope-list
       `((:description
          "\"...\": { ... }"
          :key "o"
          :mark-node t
          :nodes ("pair")
          :name "nest-pair-object"
          :template
          ("\"" (p pair "Pair") "\": " "{" n> r> n> "}" >))
         (:description
          "[ ... ]"
          :key "a"
          :mark-node t
          :nodes ("string" "array" "number" "null" "true" "false")
          :name "nest-array"
          :template
          ("[" r> "]"))))
      (pretty-print-node-name-function #'combobulate-json-pretty-print-node-name)
      (highlight-queries-default
       '(;; highlight pseudo "comments" that are often designated "//"
         ((pair key: (string (string_content) @hl.comment (:match "^//$" @hl.comment))) @hl.comment)
         ;; catch pairs where the there are duplicate key names.
         (([(object (pair key: (_) @hl.fiery) (pair key: (_) @hl.silver) (:equal @hl.silver @hl.fiery))]))))
      (procedures-edit
       `(;; edit the value field of a pair
         (:activation-nodes
          ((:nodes
            ((rule "pair"))
            :has-fields "value"
            :has-ancestor ((irule "pair"))))
          :selector (:choose
                     parent
                     :match-query
                     (:query (object (pair (_) (_) @match)+) :engine combobulate)))
         ;; edit the key field of a pair
         (:activation-nodes
          ((:nodes
            ((rule "pair"))
            :has-fields "key"
            :has-ancestor ((irule "pair"))))
          :selector (:choose
                     parent
                     :match-query
                     (:query (object (pair (_) @match)+) :engine combobulate)))))

      (procedures-sexp
       '((:activation-nodes ((:nodes ("pair"))))))
      (procedures-defun '((:activation-nodes ((:nodes ("document"))))))

      (procedures-sibling
       '(;; general navigation
         (:activation-nodes
          ((:nodes
            ((rule "array"))
            :position at
            :has-parent ((rule "array"))))
          :selector (:match-children t))
         ;; pair-wise navigation (key side)
         (:activation-nodes
          ((:nodes ("pair") :position at :has-parent ("object")))
          :selector (:match-children t))
         (:activation-nodes
          ((:nodes
            ((rule "pair"))
            :has-fields "value"
            :has-ancestor ((irule "pair"))))
          :selector (:choose
                     parent
                     :match-query
                     (:query (object (pair (_) (_) @match)+) :engine combobulate)))))

      (procedures-hierarchy
       '(;; general navigation
         (:activation-nodes
          ((:nodes (exclude (all) "string") :position at))
          :selector (:choose node :match-children t))))
      (procedures-logical '((:activation-nodes ((:nodes (all)))))))))

(define-combobulate-language
 :name json
 :language json
 :major-modes (json-mode json-ts-mode js-json-mode)
 :custom combobulate-json-definitions
 :setup-fn combobulate-json-setup)

(defun combobulate-json-setup (_))

(provide 'combobulate-json)
;;; combobulate-json.el ends here
