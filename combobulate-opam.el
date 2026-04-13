;;; combobulate-opam.el --- opam mode support for Combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Tim McGilchrist

;; Author: Tim McGilchrist
;; Keywords: ocaml, languages, opam

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

;; Combobulate support for opam package manager files using tree-sitter.
;; Opam files are structured configuration files for the OCaml package
;; manager with variables, sections, and dependency specifications.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-interface)
(require 'combobulate-rules)

(defun combobulate-opam-pretty-print-node-name (node default-name)
  "Pretty print the name of NODE."
  (combobulate-string-truncate
   (pcase (combobulate-node-type node)
     ("variable"
      (let ((name-node (combobulate-node-child-by-field node "name")))
        (if name-node
            (concat (combobulate-node-text name-node) ":")
          default-name)))
     ("section"
      (let ((kind-node (combobulate-node-child-by-field node "kind"))
            (name-node (combobulate-node-child-by-field node "name")))
        (concat (if kind-node (combobulate-node-text kind-node) "section")
                (if name-node (concat " " (combobulate-node-text name-node)) "")
                " { ... }")))
     ("option_value"
      (let ((val-node (combobulate-node-child-by-field node "value")))
        (if val-node
            (combobulate-node-text val-node)
          default-name)))
     ("ident" (combobulate-node-text node))
     ("string" (combobulate-node-text node))
     (_ default-name))
   40))

(eval-and-compile
  (defvar combobulate-opam-definitions
    '((context-nodes
       '("ident" "string" "int" "bool" "filter_ident"))
      (envelope-list nil)
      (pretty-print-node-name-function #'combobulate-opam-pretty-print-node-name)
      (highlight-queries-default nil)
      (procedures-sexp
       '((:activation-nodes ((:nodes ("variable" "section"))))))
      (procedures-defun
       '((:activation-nodes ((:nodes ("variable" "section"))))))
      (procedures-sibling
       '(;; navigate between values in a group
         (:activation-nodes
          ((:nodes ("option_value" "atom" "group"
                    "prefix_relop" "relop_value"
                    "logop_value" "pfxop_value")
            :has-parent ("group")))
          :selector (:match-children t))
         ;; navigate between values in a list (e.g. dependencies, build steps)
         (:activation-nodes
          ((:nodes ("value" "option_value" "atom" "group" "list"
                    "prefix_relop" "relop_value"
                    "logop_value" "pfxop_value")
            :has-parent ("list")))
          :selector (:match-children t))
         ;; navigate between items within a section
         (:activation-nodes
          ((:nodes ("variable" "section")
            :has-parent ("section")))
          :selector (:match-children (:match-rules ("variable" "section"))))
         ;; navigate between top-level variables, sections, and comments
         (:activation-nodes
          ((:nodes ("variable" "section" "comment")
            :has-parent ("source_file")))
          :selector (:match-children (:match-rules ("variable" "section" "comment"))))))
      (procedures-hierarchy
       '(;; navigate into variable values (e.g. depends: [...])
         (:activation-nodes
          ((:nodes ("variable") :position at))
          :selector (:choose node :match-children (:match-rules (exclude (all) "ident"))))
         ;; navigate into sections
         (:activation-nodes
          ((:nodes ("section") :position at))
          :selector (:choose node :match-children (:match-rules ("variable" "section"))))
         ;; navigate into lists and groups
         (:activation-nodes
          ((:nodes ("list" "group") :position at))
          :selector (:choose node :match-children t))
         ;; navigate into option_value (dep with constraints)
         (:activation-nodes
          ((:nodes ("option_value") :position at))
          :selector (:choose node :match-children t))
         ;; navigate from source_file into children
         (:activation-nodes
          ((:nodes ("source_file") :position at))
          :selector (:choose node :match-children t))))
      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))
      (procedures-default
       '((:activation-nodes ((:nodes ("variable" "section" "list" "group")))))))))

(define-combobulate-language
 :name opam
 :major-modes (opam-ts-mode)
 :custom combobulate-opam-definitions
 :setup-fn combobulate-opam-setup)

(defun combobulate-opam-setup (_))

(provide 'combobulate-opam)
;;; combobulate-opam.el ends here
