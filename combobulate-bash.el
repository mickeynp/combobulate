;;; combobulate-bash.el --- Bash support for combobulate  -*- lexical-binding: t; -*-

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

;; Supports the tree-sitter-bash grammar, as used by `bash-ts-mode'.
;;
;; `if', `elif' and `case' items hold their statements directly, with
;; no block node, and the condition of `elif' has no field.  The
;; procedures tell the body apart by the `then' or `)' before it.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-bash nil
  "Configuration switches for Bash"
  :group 'combobulate
  :prefix "combobulate-bash-")

(defun combobulate-bash-pretty-print-node-name (node _default-name)
  "Pretty printer for Bash nodes"
  (combobulate-string-truncate
   (string-trim (car (split-string (combobulate-node-text node) "\n")))
   40))

(eval-and-compile
  (defconst combobulate-bash--blocks
    '("program" "compound_statement" "do_group" "subshell" "else_clause"
      "command_substitution" "process_substitution")
    "Node types whose children are all statements.")

  (defconst combobulate-bash--loops
    '("for_statement" "c_style_for_statement" "while_statement")
    "Node types of the loops, whose body is a `do_group'.")

  (defconst combobulate-bash--command-parts
    '("command_name" "variable_assignment" "file_redirect" "herestring_redirect")
    "Node types in a `command' that are not its arguments.")

  (defun combobulate-bash--statement-procedures (position)
    "Return the sibling procedures for statements, activated at POSITION."
    `((:activation-nodes
       ((:nodes ("case_item") :position ,position :has-parent ("case_statement")))
       :selector (:choose parent :match-children (:match-rules ("case_item"))))
      ;; Without a selector, a condition is its own only sibling, so
      ;; it is never dragged into the body.
      (:activation-nodes
       ((:nodes ((all)) :position ,position :has-fields ("condition"))))
      (:activation-nodes
       ((:nodes ((rule "_statement")) :position ,position :has-parent ("if_statement")))
       :selector (:choose parent
                          :match-query (:query (if_statement "then" (_)+ @match)
                                               :engine combobulate
                                               :discard-rules ("elif_clause" "else_clause"))))
      (:activation-nodes
       ((:nodes ((rule "_statement")) :position ,position :has-parent ("elif_clause")))
       :selector (:choose parent
                          :match-query (:query (elif_clause "then" (_)+ @match)
                                               :engine combobulate)))
      (:activation-nodes
       ((:nodes ((rule "_statement")) :position ,position :has-parent ("case_item")))
       :selector (:choose parent
                          :match-query (:query (case_item ")" (_)+ @match)
                                               :engine combobulate)))
      (:activation-nodes
       ((:nodes ((rule "_statement")) :position ,position :has-parent ,combobulate-bash--blocks))
       :selector (:choose parent :match-children t))))

  (defvar combobulate-bash-definitions
    `((context-nodes '("word" "variable_name" "special_variable_name" "number" "string_content"))
      (procedure-discard-rules '("comment"))
      (pretty-print-node-name-function #'combobulate-bash-pretty-print-node-name)
      (procedures-defun '((:activation-nodes ((:nodes ("function_definition"))))))
      (procedures-sibling
       ;; Procedures are tried in order, each against point's node and
       ;; all its ancestors.  The statement at point comes first, then
       ;; the parts of a statement, then the statement point is in.
       '(,@(combobulate-bash--statement-procedures 'at)
         (:activation-nodes
          ((:nodes ((exclude (all) ,@combobulate-bash--command-parts)) :has-parent ("command")))
          :selector (:choose parent
                             :match-children (:discard-rules ,combobulate-bash--command-parts)))
         (:activation-nodes
          ((:nodes ((all)) :has-parent ("pipeline" "list" "array" "declaration_command"
                                         "unset_command" "variable_assignments")))
          :selector (:choose parent :match-children t))
         ,@(combobulate-bash--statement-procedures nil)))
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ("function_definition") :position at))
          :selector (:choose node
                             :match-query (:query (function_definition (compound_statement (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("if_statement") :position at))
          :selector (:choose node
                             :match-query (:query (if_statement "then" (_)+ @match)
                                                  :engine combobulate
                                                  :discard-rules ("elif_clause" "else_clause"))))
         (:activation-nodes
          ((:nodes ("elif_clause") :position at))
          :selector (:choose node
                             :match-query (:query (elif_clause "then" (_)+ @match)
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ,combobulate-bash--loops :position at))
          :selector (:choose node
                             :match-query (:query (_ (do_group (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("case_statement") :position at))
          :selector (:choose node :match-children (:match-rules ("case_item"))))
         (:activation-nodes
          ((:nodes ("case_item") :position at))
          :selector (:choose node
                             :match-query (:query (case_item ")" (_)+ @match)
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("command") :position at))
          :selector (:choose node
                             :match-children (:discard-rules ,combobulate-bash--command-parts)))
         (:activation-nodes
          ((:nodes ((all)) :position at))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name bash
 :major-modes (bash-ts-mode)
 :custom combobulate-bash-definitions
 :setup-fn combobulate-bash-setup)

(defun combobulate-bash-setup (_))

(defun combobulate-bash--drag (command arg)
  "Run the drag COMMAND with ARG, unless point is not on one of the siblings.

The condition of `elif' has no field, so the procedure for the
body activates on it too, and dragging would swap it into the body."
  (with-navigation-nodes (:procedures (combobulate-read procedures-sibling))
    (when-let* ((real (lambda (node) (if (combobulate-proxy-node-p node)
                                         (combobulate-proxy-node-to-real-node node)
                                       node)))
                (nearest (combobulate--get-nearest-navigable-node))
                (self (funcall real (or (combobulate-nav-get-self-sibling nearest) nearest)))
                (siblings (mapcar real (combobulate-nav-get-siblings self))))
      (unless (seq-position siblings self #'combobulate-node-eq)
        (user-error "Cannot drag %s" (combobulate-pretty-print-node self)))))
  (funcall command arg))

(defun combobulate-bash-drag-up (&optional arg)
  "Like `combobulate-drag-up', but refuse to drag a node out of its siblings."
  (interactive "^p")
  (combobulate-bash--drag #'combobulate-drag-up arg))

(defun combobulate-bash-drag-down (&optional arg)
  "Like `combobulate-drag-down', but refuse to drag a node out of its siblings."
  (interactive "^p")
  (combobulate-bash--drag #'combobulate-drag-down arg))

(define-key combobulate-bash-map [remap combobulate-drag-up] #'combobulate-bash-drag-up)
(define-key combobulate-bash-map [remap combobulate-drag-down] #'combobulate-bash-drag-down)

(provide 'combobulate-bash)
;;; combobulate-bash.el ends here
