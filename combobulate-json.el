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
               (concat (combobulate-node-text node))
               40))
    (_ default-name)))

(defun combobulate-json-setup (_)
  (setq combobulate-navigation-context-nodes
        '("string" "string_content" "number" "null" "true" "false"))

  (setq combobulate-manipulation-envelopes
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

  (setq combobulate-pretty-print-node-name-function #'combobulate-json-pretty-print-node-name)
  (setq combobulate-manipulation-trim-whitespace 'backward)
  (setq combobulate-manipulation-trim-empty-lines t)
  (setq combobulate-highlight-queries-default
        '(;; highlight pseudo "comments" that are often designated "//"
          ((pair key: (string (string_content) @hl.comment (:match "^//$" @hl.comment))) @hl.comment)))
  (setq combobulate-manipulation-edit-procedures
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
  (setq combobulate-navigation-sibling-skip-prefix t)
  (setq combobulate-navigation-sexp-procedures
        '((:activation-nodes ((:nodes ("pair"))))))
  (setq combobulate-navigation-defun-procedures '((:activation-nodes ((:nodes ("document"))))))

  (setq combobulate-navigation-sibling-procedures
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

  (setq combobulate-navigation-parent-child-procedures
        '(;; general navigation
          (:activation-nodes
           ((:nodes (exclude (all) "string") :position at))
           :selector (:choose node :match-children t))))
  (setq combobulate-navigation-logical-procedures '((:activation-nodes ((:nodes (all)))))))

(provide 'combobulate-json)
;;; combobulate-json.el ends here
