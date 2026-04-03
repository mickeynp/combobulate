;;; combobulate-dune.el --- Dune mode support for Combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Tim McGilchrist

;; Author: Tim McGilchrist
;; Keywords: ocaml, languages, dune

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

;; Combobulate support for dune build system files using tree-sitter.
;; Dune files are s-expression based configuration files used by the
;; OCaml build system.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-manipulation)
(require 'combobulate-interface)
(require 'combobulate-rules)

(defun combobulate-dune-pretty-print-node-name (node default-name)
  "Pretty print the name of NODE."
  (combobulate-string-truncate
   (pcase (combobulate-node-type node)
     ("stanza"
      (let ((name-node (combobulate-node-child node 0)))
        (if name-node
            (concat "(" (combobulate-node-text name-node) " ...)")
          default-name)))
     ("field_name" (combobulate-node-text node))
     ("stanza_name" (combobulate-node-text node))
     ("module_name" (combobulate-node-text node))
     ("library_name" (combobulate-node-text node))
     ("package_name" (combobulate-node-text node))
     ("quoted_string" (combobulate-node-text node))
     (_ default-name))
   40))

(eval-and-compile
  (defvar combobulate-dune-definitions
    '((context-nodes
       '("module_name" "library_name" "package_name" "public_name"
         "file_name" "alias_name" "lock_name" "quoted_string"
         "multiline_string" "shell_command" "named_variable"
         "stanza_name" "field_name" "action_name"))
      (envelope-list nil)
      (pretty-print-node-name-function #'combobulate-dune-pretty-print-node-name)
      (highlight-queries-default nil)
      (procedures-sexp
       '((:activation-nodes ((:nodes ("stanza" "sexp" "action"))))))
      (procedures-defun
       '((:activation-nodes ((:nodes ("stanza"))))))
      (procedures-sibling
       '(;; navigate between package dependency components
         (:activation-nodes
          ((:nodes ("package_name" "version_constraint")
            :has-parent ("package_dep")))
          :selector (:match-children t))
         ;; navigate between action arguments
         (:activation-nodes
          ((:nodes ("action" "file_name" "quoted_string"
                    "shell_command")
            :has-parent ("action")))
          :selector (:match-children t))
         ;; navigate between children within a sexp
         (:activation-nodes
          ((:nodes ("sexp" "quoted_string" "multiline_string"
                    "module_name" "library_name" "file_name")
            :has-parent ("sexp")))
          :selector (:match-children t))
         ;; navigate between field names within a stanza
         ;; (e.g. name, synopsis, depends in a package stanza)
         (:activation-nodes
          ((:nodes ("field_name")
            :has-parent ("stanza")))
          :selector (:match-children (:match-rules ("field_name"))))
         ;; navigate between value nodes within a stanza
         ;; (e.g. library_name siblings in libraries, package_dep
         ;; siblings in depends)
         (:activation-nodes
          ((:nodes ("module_name" "library_name" "package_name"
                    "public_name" "file_name" "sexp" "action"
                    "sexps1" "package_dep")
            :has-parent ("stanza")))
          :selector (:match-children (:match-rules
                                      ("module_name" "library_name"
                                       "package_name" "public_name"
                                       "file_name" "sexp" "action"
                                       "sexps1" "package_dep"))))
         ;; navigate between top-level stanzas
         (:activation-nodes
          ((:nodes ("stanza")
            :has-parent ("source_file")))
          :selector (:match-children (:match-rules ("stanza"))))))
      (procedures-hierarchy
       '(;; navigate into stanzas
         (:activation-nodes
          ((:nodes ("stanza") :position at))
          :selector (:choose node :match-children
                             (:match-rules (exclude (all) "stanza_name"))))
         ;; navigate into sexps and actions
         (:activation-nodes
          ((:nodes ("sexp" "action") :position at))
          :selector (:choose node :match-children t))
         ;; navigate into nested stanzas (subdir)
         (:activation-nodes
          ((:nodes ("source_file") :position at))
          :selector (:choose node :match-children (:match-rules ("stanza"))))))
      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))
      (procedures-default
       '((:activation-nodes ((:nodes ("stanza" "sexp" "action")))))))))

(define-combobulate-language
 :name dune
 :language dune
 :major-modes (neocaml-dune-mode)
 :custom combobulate-dune-definitions
 :setup-fn combobulate-dune-setup)

(defun combobulate-dune-setup (_))

(provide 'combobulate-dune)
;;; combobulate-dune.el ends here
