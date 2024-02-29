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
(require 'combobulate-manipulation)
(declare-function combobulate--mark-node "combobulate-manipulation")
(declare-function combobulate-indent-region "combobulate-manipulation")

(defvar combobulate-python-indent--direction nil)

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

(defcustom combobulate-python-indent-mark-region nil
  "Mark the region when indenting and leave it enabled after.

When non-nil, Combobulate will mark the region when indenting
with `combobulate-python-indent-for-tab-command'.

When nil, the mark is instead deactivated after indenting."
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

(defun combobulate-proffer-indentation-1 (node)
  (combobulate-indent-region
   ;; we want to mark from the beginning of line
   (save-excursion
     (goto-char (combobulate-node-start node))
     (skip-chars-backward combobulate-skip-prefix-regexp (line-beginning-position))
     (point))
   (combobulate-node-end node)
   ;; no baseline target
   0
   (combobulate-proxy-node-extra node))
  ;; this ensures point is at the beginning of the node but also after
  ;; the indentation.
  (combobulate-move-to-node node)
  (back-to-indentation))

(defun combobulate-proffer-indentation (node)
  "Intelligently indent the region or NODE at point."
  (interactive)
  (let* ((indentation (save-excursion
                        (combobulate--goto-node node)
                        (python-indent-calculate-levels)))
         (indent-nodes (mapcar
                        (lambda (level)
                          (save-excursion
                            (let ((proxy-node
                                   ;; A region may, potentially, signify a valid
                                   ;; node range, but it is unlikely. When a user
                                   ;; wants to indent a region they want to --
                                   ;; presumably -- indent code that may not be
                                   ;; syntactically sound. For that we'll create a
                                   ;; special proxy node that will be used to
                                   ;; indent the region.
                                   (if (use-region-p)
                                       (combobulate-proxy-node-make-from-range (region-beginning) (region-end))
                                     ;; if we're not dealing with a region, we
                                     ;; make a proxy node for the closest node.
                                     (combobulate-proxy-node-make-from-nodes node))))
                              (setf (combobulate-proxy-node-extra proxy-node)
                                    (- (current-indentation) level))
                              proxy-node)))
                        (python-indent-calculate-levels)))
         (current-position (1+ (or (seq-position indentation (current-indentation)) 0)))
         (number-of-levels (length (python-indent-calculate-levels)))
         (at-last-level (= number-of-levels current-position)))
    (when-let (selected-node (combobulate-proffer-choices
                              (if at-last-level (reverse indent-nodes) indent-nodes)
                              (lambda-slots (refactor-id current-node)
                                (combobulate-refactor (:id refactor-id)
                                  (mark-node-highlighted current-node)
                                  (combobulate-proffer-indentation-1 current-node)))
                              ;; Try to pick a sensible starting index
                              ;; based on whether we're at the end,
                              ;; taking into account of the fact that
                              ;; there might only be one node to
                              ;; choose from.
                              :start-index (if at-last-level
                                               (min (1- (length indent-nodes)) 1)
                                             (mod current-position number-of-levels))
                              :flash-node t
                              ;; do not filter unique nodes. all our nodes are conceptually
                              ;; identical except for the `extra' field.
                              :allow-numeric-selection t
                              :unique-only nil))
      (combobulate-proffer-indentation-1 selected-node))))

(defun combobulate-python-envelope-deindent-level ()
  "Determine the next-closest indentation level to deindent to."
  (car-safe (last (seq-take-while (lambda (num) (< num (current-column)))
                                  (python-indent-calculate-levels)))))


(defun combobulate-python-indent-for-tab-command (&optional arg)
  "Wrapper for `indent-for-tab-command' that adds advanced indentation."
  (interactive "P")
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
    (let ((node (combobulate--get-nearest-navigable-node)))
      (cond
       ((use-region-p)
        ;; ensure point is at the beginning of the region
        (when (> (point) (mark))
          (exchange-point-and-mark))
        (combobulate-proffer-indentation node)
        ;; toggle the region on and off so it doesn't get deactivated
        (setq deactivate-mark nil)
        (activate-mark))
       ;; we need to handle blank lines as tabbing on a blank line should
       ;; default to the regular python indentation mechanism.
       ((save-excursion
          (beginning-of-line)
          (looking-at-p "[[:space:]]*$"))
        (indent-for-tab-command arg))
       ;; if we're at the beginning of a node, we want to indent it.
       ((and combobulate-python-indent-blocks-dwim (combobulate-point-at-beginning-of-node-p node))
        (combobulate-proffer-indentation node))
       ;; for everything else, use the regular indentation mechanism.
       (t (indent-for-tab-command arg))))))

(defun combobulate-python-setup (_)
  (setq combobulate-navigation-context-nodes '("identifier"))

  ;; do not indent envelopes.
  (setq combobulate-envelope-indent-region-function nil)
  (when combobulate-python-smart-indent
    ;; Override `indent-for-tab-command'
    (local-set-key [remap indent-for-tab-command] #'combobulate-python-indent-for-tab-command))
  ;; install a handful of useful highlighting rules.
  (setq combobulate-highlight-queries-default
        '(;; highlight breakpoint function calls
          (((call (identifier) @hl.fiery (:match "^breakpoint$" @hl.fiery))))
          ;; catch trailing commas that inadvertently turn expressions into tuples
          ((expression_list (_)+ "," @hl.gold :anchor))))
  (setq combobulate-manipulation-indent-after-edit nil)
  (setq combobulate-pretty-print-node-name-function #'combobulate-python-pretty-print-node-name)
  (let ((statement-nodes
         (append (combobulate-production-rules-get "_compound_statement")
                 (combobulate-production-rules-get "_simple_statement")
                 '("expression_statement" "block"))))
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
             "if ...: ... [else: ...]"
             :key "bi"
             :mark-node t
             :nodes ,statement-nodes
             :name "nest-if-else"
             :template
             ("if " @ (p True "Bool") ":" n>
              (choice* :missing
                       nil
                       :rest
                       ((save-column r> n) < "else:" (save-column n> "pass" n))
                       :name "if-block")
              (choice* :missing
                       nil
                       :rest
                       (@ (save-column "pass" n> "else:" n> r> n))
                       :name "else-block")))
            (:description
             "try ... except ...: ..."
             :key "bte"
             :mark-node t
             :nodes ,statement-nodes
             :name "nest-try-except"
             :template
             ((save-column
               "try:" n>
               (choice* :name "statement-block"
                        :missing
                        (@ "pass")
                        :rest
                        (r>))
               n)
              "except " (p Exception "Exception") ":" n>
              (choice* :name "handler-block"
                       :missing
                       (@ "pass" n>)
                       :rest
                       (r> n))))
            (:description
             "try ... finally: ..."
             :key "btf"
             :mark-node t
             :nodes ,statement-nodes
             :name "nest-try-finally"
             :template
             ((save-column
               @ "try:" n>
               (choice* :missing (@ "pass") :rest (r>) :name "try-block") n)
              "finally:" n>
              (choice* :missing (@ "pass") :rest (r>) :name "finally-block")))
            (:description
             "def ...():"
             :key "bd"
             :mark-node t
             :nodes ,statement-nodes
             :name "nest-def"
             :template
             ("def " (p name "Name") "(" @ ")" ":" n>
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
             "with ...:"
             :key "bW"
             :mark-node t
             :nodes ,statement-nodes
             :name "nest-with"
             :template
             ("with " @ ":" n>
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

  (add-to-list 'python-indent-trigger-commands 'combobulate-python-indent-for-tab-command)
  (setq combobulate-manipulation-edit-procedures
        '(;; edit comments in blocks
          (:activation-nodes
           ((:nodes ("comment") :has-parent ("block")))
           :selector (:match-query (:query (block (comment)+ @match)
                                           :engine combobulate)))
          ;; edit pairs in dictionaries
          (:activation-nodes
           ((:nodes ("pair") :has-parent "dictionary")
            (:nodes ("dictionary")))
           :selector (:match-query (:query (dictionary (pair)+ @match)
                                           :engine combobulate)))
          ;; edit parameters in functions
          (:activation-nodes
           ((:nodes ("function_definition")))
           :selector (:match-query (:query (function_definition (parameters (_)+ @match))
                                           :engine combobulate)))
          ;; edit elements in containers and blocks
          (:activation-nodes
           ((:nodes ("block" "tuple_pattern" "set" "list" "tuple")))
           :selector (:choose
                      node
                      :match-query (:query ((_) (_)+ @match)
                                           :engine combobulate))
           :selector (:match-children t))
          ;; edit arguments in calls
          (:activation-nodes
           ((:nodes ("argument_list")))
           :selector (:match-query (:query ((argument_list) (_)+ @match)
                                           :engine combobulate)))
          ;; edit imports
          (:activation-nodes
           ((:nodes "import_from_statement" :find-parent "module"))
           :selector (:match-query (:query (import_from_statement name: (dotted_name)+ @match)
                                           :engine combobulate)))))

  (setq combobulate-calculate-indent-function #'combobulate-python-calculate-indent)
  (setq combobulate-envelope-deindent-function #'combobulate-python-envelope-deindent-level)
  (setq combobulate-navigation-defun-procedures
        '((:activation-nodes ((:nodes ("class_definition" "function_definition" "decorated_definition" "lambda"))))))
  (setq combobulate-navigation-sexp-procedures
        '((:activation-nodes ((:nodes ("function_definition"  "class_definition" "lambda"
                                       "for_in_clause" "string" "decorated_definition"))))))
  (setq combobulate-navigation-sibling-procedures
        '((:activation-nodes
           ((:nodes
             ("string_content" "interpolation")
             :has-parent ("string")))
           :selector (:match-children
                      (:discard-rules
                       ("string_start" "string_end")
                       :default-mark @match)))
          (:activation-nodes
           ((:nodes
             ;; pattern is a special supertype. It is not a node in the CST.
             ((rule "pattern"))
             ;; Note that we do not find all the parents of pattern
             ;; but only a couple. The main reason is that otherwise
             ;; they'd become potential next/prev siblings in a block
             ;; and that's generally not what people expect when
             ;; they're navigating siblings in a block. By limiting
             ;; ourselves to explicit tuples/lists, the user would
             ;; have to enter these nodes explicitly to navigate them.
             :has-parent ("tuple_pattern" "list_pattern"))
            (:nodes
             ((rule "import_from_statement"))
             :has-parent ("import_from_statement"))
            (:nodes
             ((rule "dictionary"))
             :has-parent ("dictionary"))
            (:nodes
             ((rule "set") (rule "tuple") (rule "list"))
             :has-parent ("set" "tuple" "list"))
            (:nodes
             ((rule "parameter")
              (rule "argument_list")
              (rule "expression")
              (rule "expression_list")
              (rule "primary_expression"))
             :has-parent ("parameters" "lambda_parameters" "argument_list" "expression_list")))
           :selector (:match-children t))
          (:activation-nodes
           ((:nodes
             ((rule "_simple_statement")
              (rule "_compound_statement")
              (rule "module")
              "module" "case_clause")
             :has-parent ("case_clause" "match_statement" "module" "block")))
           :selector (:match-children (:discard-rules ("block"))))))

  (setq combobulate-navigation-parent-child-procedures
        '(;; statements are treated with `at' so you can descend into sub-statements.
          (:activation-nodes
           ((:nodes ((rule "_compound_statement")
                     ;; not in compound statement
                     "case_clause")
                    :position at))
           :selector (:choose node
                              :match-children (:match-rules ("block"))))
          (:activation-nodes
           ((:nodes ((rule "lambda"))
                    :position at))
           :selector (:choose node
                              :match-children (:match-rules (rule "lambda" :body))))
          (:activation-nodes
           ((:nodes ((all)) :has-parent ((all))))
           :selector (:choose node
                              :match-children (:discard-rules ("block"))))))
  (setq combobulate-navigation-logical-procedures
        '((:activation-nodes ((:nodes (all)))))))

(provide 'combobulate-python)
;;; combobulate-python.el ends here
