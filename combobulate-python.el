;;; combobulate-python.el --- python-specific features for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-23  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
;; Package-Requires: ((emacs "29"))
;; Version: 0.1
;; Homepage: https://www.github.com/mickeynp/combobulate
;; Keywords: convenience, tools, languages

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

(require 'python)
(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-interface)
(require 'combobulate-rules)

(declare-function combobulate--mark-node "combobulate-manipulation")
(declare-function combobulate-indent-region "combobulate-manipulation")


(defgroup combobulate-python nil
  "Configuration switches for Python."
  :group 'combobulate
  :prefix "combobulate-python-")

(defcustom combobulate-python-smart-indent t
  "Smarter indentation handling.

Greatly improves indentation handling when you indent
regions. Combobulate will automatically indent the region and
preserve the existing indentation. You can then cycle through
indentation levels to pick the right one.

If `combobulate-python-indent-blocks-dwim' is non-nil, then
Combobulate will automatically pick the code block immediately
ahead of point. You can use this to cycle through the indentation
of blocks of code: functions, for statements, etc.

This works by remapping `indent-for-tab-command' to
`combobulate-python-indent-for-tab-command'."
  :type 'boolean
  :group 'combobulate-python)

(defcustom combobulate-python-indent-blocks-dwim t
  "Indent a whole block if point is on it instead of the line.

With point at the beginning of a block -- such as a for statement
-- Combobulate will instead indent the block instead of just the
line when you press
\\[combobulate-python-indent-for-tab-command]."
  :type 'boolean)

(defun combobulate-python--get-definition (node)
  (string-join
   (combobulate-query-node-text
    (pcase (combobulate-node-type node)
      ("function_definition"
       '((_) name: (_) @name parameters: (_) @args))
      ("class_definition"
       '((_) name: (_) @name superclasses: (_) @args)))
    node
    t)
   ""))

(defun combobulate-python-pretty-print-node-name (node default-name)
  "Pretty printer for Python nodes"
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("function_definition" (concat "def " (combobulate-python--get-definition node)))
      ("class_definition" (concat "class " (combobulate-python--get-definition node)))
      (_ default-name)))
   40))

(defun combobulate-python--display-indicator ()
  (let* ((levels (python-indent-calculate-levels))
         (total-levels (length levels))
         (matched-level (member (current-indentation) levels))
         (current-level (abs (- (length matched-level) total-levels))))
    (combobulate-display-indicator current-level total-levels)))

(defvar-local combobulate-python-indent-cycle nil)

(defun combobulate-python-indent-region (start end &optional columns)
  "Indent a python region between START and END to COLUMNS.

This function is designed to be called from
`indent-region-function' and (indirectly) through
\\[combobulate-python-indent-for-tab-command]."
  (let ((deactivate-mark nil))
    (when combobulate-python-indent-cycle
      (save-mark-and-excursion
        (when (and (use-region-p) (> (point) (mark)))
          (exchange-point-and-mark))
        (skip-chars-backward combobulate-skip-prefix-regexp
                             (line-beginning-position))
        (when (bolp)
          (setq start (point)))
        (combobulate-indent-region
         start end (or columns (pop combobulate-python-indent-cycle)))))))

(defvar-local combobulate-python-indent-cycle nil
  "List of indentation columns to cycle through with
\\[combobulate-python-indent-for-tab-command].")


(defun combobulate-python-maybe-indent-block-at-point ()
  "Maybe indent the block at point.

Returns a non-nil value to indicate the indentation took place."
  (with-navigation-nodes
      (:nodes (append
               ;; rules that trigger indentation
               (combobulate-production-rules-get "_simple_statement")
               (combobulate-production-rules-get "_compound_statement"))
              ;; do not skip prefix if we have a region active. the
              ;; reason for that is that skipping forward with a
              ;; marked region can bork the indentation mechanism as
              ;; we can only effectively indent with whole lines.
              :skip-prefix (not (use-region-p))
              :skip-newline nil)
    (when-let ((node (combobulate--get-nearest-navigable-node)))
      (when (and (not (use-region-p))
                 combobulate-python-indent-blocks-dwim
                 (combobulate-point-at-beginning-of-node-p
                  (combobulate--get-nearest-navigable-node)))
        (combobulate--mark-node node t t)))))

(defun combobulate-python-calculate-indent (pos)
  (let ((calculated-indentation (save-excursion
                                  (goto-char pos)
                                  (combobulate-filter-nodes
                                   (combobulate-get-parents
                                    (combobulate-node-at-point))
                                   :keep-types
                                   '("block"
                                     ;; required because, for some inexplicable reason, the
                                     ;; python grammar does not consider a match-case statement
                                     ;; to consist of a case clause and a block clause unlike
                                     ;; literally everything else.
                                     "case_clause")))))
    (if (null calculated-indentation)
        (current-indentation)
      (* python-indent-offset (length calculated-indentation)))))

(defun combobulate-python-indent-for-tab-command (&optional arg)
  "Proxy command for `indent-for-tab-command' that keeps region active.

This command preserves the region if it is active. Subsequent
indent commands cycle through all valid indentation stops."
  (interactive "P")
  (combobulate-python-maybe-indent-block-at-point)
  (if (use-region-p)
      (let ((has-active-region (use-region-p))
            (levels) (rlevels)
            (current-level))
        ;; work out the correct indentation point to use as a baseline:
        ;; the top-most place in the region.
        (save-mark-and-excursion
          (when (and has-active-region (> (point) (mark)))
            (exchange-point-and-mark))
          (setq levels (python-indent-calculate-levels))
          (setq rlevels (reverse (python-indent-calculate-levels)))
          (setq current-level (if (member (current-indentation) levels)
                                  (current-indentation)
                                (python-indent-calculate-indentation)))
          ;; if the `this-command' and `last-command' match, then we've been
          ;; requesting indentation multiple times in a row. If not, we're
          ;; doing this for the first time, and so we must calculate the
          ;; indentation offsets.
          (unless (and (eq last-command this-command) has-active-region)
            ;; avoid loops if there is only one entry.
            (when (> (length levels) 1)
              ;; Build a circular loop of `levels' so we can tab ad infinitum
              ;; and cycle through the options.
              (setq combobulate-python-indent-cycle (if (= (apply #'max levels) current-level)
                                                        (nconc rlevels rlevels)
                                                      (nconc levels levels)))
              (while (and (= current-level (car combobulate-python-indent-cycle))
                          (member current-level levels))
                (pop combobulate-python-indent-cycle))))
          (indent-for-tab-command arg)
          (combobulate-message
           (concat (combobulate-python--display-indicator)
                   " "
                   (substitute-command-keys
                    "Press \\[combobulate-python-indent-for-tab-command] \
again to cycle indentation."))))
        (when has-active-region
          (setq deactivate-mark nil)))
    (indent-for-tab-command arg)))

(defun combobulate-python-setup (_)
  ;; do not indent envelopes.
  (setq combobulate-envelope-indent-region-function nil)
  (when combobulate-python-smart-indent
    ;; Override `indent-for-tab-command'
    (local-set-key [remap indent-for-tab-command] #'combobulate-python-indent-for-tab-command))
  (setq indent-region-function 'combobulate-python-indent-region)
  (setq combobulate-manipulation-indent-after-edit nil)
  (setq combobulate-pretty-print-node-name-function #'combobulate-python-pretty-print-node-name)
  (setq combobulate-manipulation-splicing-procedures
        `(
          (:activation-nodes
           ((:node
             ,(append (combobulate-production-rules-get "_simple_statement")
                      (combobulate-production-rules-get "_compound_statement")
                      (combobulate-production-rules-get "if_statement")
                      (combobulate-production-rules-get "try_statement")
                      '("case_clause"))
             :find-base-rule-parent t
             :position at-or-in))
           :match-siblings (:keep-parent nil))))

  (let ((statement-nodes
         (append (combobulate-production-rules-get "_compound_statement")
                 '("block"))))
    (setq combobulate-manipulation-envelopes
          `((:description
             "( ... )"
             :key "("
             :extra-key "M-("
             :mark-node t
             :nodes ,(append (combobulate-production-rules-get "primary_expression")
                             (combobulate-production-rules-get "expression"))
             :name "wrap-parentheses"
             :template (@ "(" r ")"))
            (:description
             "Decorate class or function"
             :key "@"
             :mark-node nil
             :nodes ("function_definition" "class_definition")
             :name "decorate"
             :template ((p @decorator "Decorator name"
                           (lambda (text)
                             (if (string-prefix-p "@" text)
                                 text
                               (concat "@" text))))
                        n>))
            (:description
             "if ...:"
             :key "bi"
             :mark-node t
             :nodes ,statement-nodes
             :name "nest-if"
             :template
             ("if " @ ":" n>
              r>))
            (:description
             "for ...:"
             :key "bf"
             :mark-node t
             :nodes ,statement-nodes
             :name "nest-for"
             :template
             ("for " @ ":" n>
              r>))
            (:description
             "while ...:"
             :key "bw"
             :mark-node t
             :nodes ,statement-nodes
             :name "nest-while"
             :template
             ("while " @ ":" n>
              r>)))))

  (push 'combobulate-python-indent-for-tab-command python-indent-trigger-commands)
  (setq combobulate-manipulation-edit-procedures
        '(;; edit comments in blocks
          (:activation-nodes
           ((:node "comment" :find-parent ("block") :position at-or-in))
           :match-query (block (comment)+ @match))
          ;; edit pairs in dictionaries
          (:activation-nodes
           ((:node "pair" :find-parent "dictionary" :position at-or-in)
            (:node "dictionary" :position at-or-in))
           :match-query (dictionary (pair)+ @match)
           :remove-types ("comment"))
          ;; edit parameters in functions
          (:activation-nodes
           ((:node "function_definition" :position at-or-in))
           :match-query (function_definition (parameters (_)+ @match))
           :remove-types ("comment"))
          ;; edit elements in containers and blocks
          (:activation-nodes
           ((:node ("block" "tuple_pattern" "set" "list" "tuple") :position at-or-in))
           :match-query ((_) (_)+ @match)
           ;; :match-children t
           :remove-types ("comment"))
          ;; edit arguments in calls
          (:activation-nodes
           ((:node "argument_list" :position at-or-in))
           :match-query ((argument_list) (_)+ @match)
           :remove-types ("comment"))
          ;; edit imports
          (:activation-nodes
           ((:node "import_from_statement" :position at-or-in :find-parent "module"))
           :match-query (import_from_statement name: (dotted_name)+ @match))))

  (setq combobulate-manipulation-indent-method 'first)
  (setq combobulate-calculate-indent-function #'combobulate-python-calculate-indent)
  (setq combobulate-navigation-defun-nodes '("class_definition" "function_definition" "lambda"))
  (setq combobulate-navigation-sexp-nodes '("function_definition"  "class_definition" "lambda"
                                            "for_in_clause"))
  (setq combobulate-navigation-drag-parent-nodes '("if_statement" "function_definition"
                                                   "module" "match_statement" "dictionary"
                                                   "case_clause" "list" "while_statement" "tuple"
                                                   "try_statement" "class_definition"
                                                   "argument_list" "import_from_statement"
                                                   "for_statement" "parameters"))
  (setq combobulate-navigation-sibling-procedures
        `((:activation-nodes
           ((:node
             ,(combobulate-production-rules-get "import_from_statement")
             :position at-or-in
             :find-immediate-parent ("import_from_statement"))
            (:node
             ,(combobulate-production-rules-get "dictionary")
             :position at-or-in
             :find-immediate-parent ("dictionary"))
            (:node
             ,(append
               (combobulate-production-rules-get "primary_expression")
               (combobulate-production-rules-get "expression"))
             :position at-or-in
             :find-immediate-parent ("set" "tuple" "list"))
            (:node
             ,(append
               (combobulate-production-rules-get "parameter")
               (combobulate-production-rules-get "argument_list")
               (combobulate-production-rules-get "expression"))
             :position at-or-in
             :find-immediate-parent ("parameters" "argument_list")))
           :match-children t
           :remove-types ("comment"))
          (:activation-nodes
           ((:node
             ,(append (combobulate-production-rules-get "_simple_statement")
                      (combobulate-production-rules-get "_compound_statement")
                      (combobulate-production-rules-get "module")
                      '("module" "comment" "case_clause"))
             :position at-or-in
             :find-immediate-parent ("case_clause" "match_statement" "module" "block")))
           :remove-types ("comment")
           :match-children t)))
  (combobulate-production-rules-set '("argument_list"
                                      :included-fields (:*unnamed*)
                                      :expand-rules (("expression" :all t)
                                                     ("primary_expression" :all t))))
  (combobulate-production-rules-set '("function_definition"
                                      :included-fields (:body)
                                      :expand-nodes (("block" :all t))))
  (combobulate-production-rules-set '("class_definition"
                                      :included-fields (:body)
                                      :expand-nodes (("block" :all t))))
  (combobulate-production-rules-set '("parameters"
                                      :included-fields (:*unnamed*)
                                      :expand-rules (("parameter" :all t))))
  (combobulate-production-rules-set '("if_statement"
                                      :included-fields (:consequence :alternative)
                                      :expand-nodes (("block" :all t))))
  (combobulate-production-rules-set '("for_statement"
                                      :included-fields (:body)
                                      :expand-nodes (("block" :all t))))
  (combobulate-production-rules-set '("match_statement"
                                      :included-fields (:alternative)))
  (combobulate-production-rules-set '("try_statement"
                                      :included-fields (:body :*unnamed*)
                                      :expand-nodes (("block" :all t))))
  (combobulate-production-rules-set '("while_statement"
                                      :included-fields (:body)
                                      :expand-nodes (("block" :all t))))
  (combobulate-production-rules-set '("with_statement"
                                      :included-fields (:body)
                                      :expand-nodes (("block" :all t))))
  (combobulate-production-rules-set '("case_clause"
                                      :included-fields (:consequence)
                                      :expand-nodes (("block" :all t))))
  (combobulate-production-rules-set '("import_from_statement"
                                      :included-fields (:name)
                                      :expand-rules (("dotted_name" :all t))))
  (combobulate-production-rules-set '("list"
                                      :all t
                                      :expand-rules (("expression" :all t)
                                                     ("primary_expression" :all t))))
  (setq combobulate-navigation-parent-child-nodes
        (append
         (combobulate-production-rules-get "_simple_statement")
         (combobulate-production-rules-get "_compound_statement")
         '("module" "dictionary" "except_clause" "for_in_clause" "finally_clause" "elif_clause"
           "list" "call" "tuple" "string" "block" "case_clause" "set")))
  (setq combobulate-navigation-logical-nodes
        (append
         (combobulate-production-rules-get "primary_expression")
         (combobulate-production-rules-get "expression")
         combobulate-navigation-default-nodes))
  (setq combobulate-navigation-default-nodes
        (seq-uniq (append
                   combobulate-navigation-logical-nodes
                   combobulate-navigation-parent-child-nodes))))

(provide 'combobulate-python)
;;; combobulate-python.el ends here
