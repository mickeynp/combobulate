;;; combobulate-erlang.el --- erlang support for combobulate  -*- lexical-binding: t; -*-

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

;; Supports the WhatsApp tree-sitter-erlang grammar, as used by
;; `erlang-ts-mode'.
;;
;; In that grammar each clause of a function is its own `fun_decl'
;; form, so the clauses of `area/1' are siblings at the top level.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)

(defgroup combobulate-erlang nil
  "Configuration switches for Erlang"
  :group 'combobulate
  :prefix "combobulate-erlang-")

(defun combobulate-erlang-pretty-print-node-name (node default-name)
  "Pretty printer for Erlang nodes"
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("fun_decl"
       (let* ((clause (combobulate-node-child-by-field node "clause"))
              (name (combobulate-node-text (combobulate-node-child-by-field clause "name")))
              (args (combobulate-node-child-by-field clause "args")))
         (format "%s/%d" name (treesit-node-child-count args t))))
      (_ default-name)))
   40))

(eval-and-compile
  (defconst combobulate-erlang--clause-types
    '("cr_clause" "if_clause" "fun_clause" "catch_clause" "receive_after" "try_after")
    "Node types that are clauses of a `case', `receive', `if', `fun' or `try'.")

  (defconst combobulate-erlang--wrappers
    '("clause_body" "expr_args" "var_args" "macro_call_args" "macro_expr"
      "guard" "guard_clause" "lc_exprs")
    "Node types that only group other nodes, so up skips them.")

  (defvar combobulate-erlang-definitions
    `((context-nodes
       '("atom" "var" "integer" "float" "string" "char"))
      (pretty-print-node-name-function #'combobulate-erlang-pretty-print-node-name)
      (procedures-defun
       '((:activation-nodes ((:nodes ("fun_decl"))))))
      (procedures-sibling
       '((:activation-nodes
          ((:nodes ,combobulate-erlang--clause-types
                   :position at
                   :has-parent ("case_expr" "receive_expr" "if_expr" "try_expr" "anonymous_fun")))
          :selector (:choose parent
                             :match-children (:match-rules ,combobulate-erlang--clause-types)))
         (:activation-nodes
          ((:nodes ("record_field")
                   :position at
                   :has-parent ("record_expr" "record_update_expr" "record_decl")))
          :selector (:choose parent :match-children (:match-rules ("record_field"))))
         (:activation-nodes
          ((:nodes ("map_field")
                   :position at
                   :has-parent ("map_expr" "map_expr_update")))
          :selector (:choose parent :match-children (:match-rules ("map_field"))))
         (:activation-nodes
          ((:nodes ((exclude (all) ,@combobulate-erlang--clause-types))
                   :position at
                   :has-parent ("try_expr")))
          :selector (:choose parent
                             :match-children (:discard-rules ,combobulate-erlang--clause-types)))
         (:activation-nodes
          ((:nodes ((all))
                   :has-parent ("source_file" "clause_body" "block_expr" "try_after"
                                "expr_args" "var_args" "macro_call_args"
                                "list" "tuple" "binary" "guard" "guard_clause" "lc_exprs"
                                "export_attribute" "export_type_attribute")))
          :selector (:choose parent :match-children t))))
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ("fun_decl") :position at))
          :selector (:choose node
                             :match-query
                             (:query (fun_decl (function_clause (clause_body (_)+ @match)))
                                     :engine combobulate)))
         (:activation-nodes
          ((:nodes ("function_clause" "cr_clause" "if_clause" "fun_clause"
                    "catch_clause" "receive_after")
                   :position at))
          :selector (:choose node
                             :match-query (:query (_ (clause_body (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ("case_expr" "receive_expr" "if_expr" "anonymous_fun") :position at))
          :selector (:choose node
                             :match-children (:match-rules ,combobulate-erlang--clause-types)))
         (:activation-nodes
          ((:nodes ("call") :position at))
          :selector (:choose node
                             :match-query (:query (call (expr_args (_)+ @match))
                                                  :engine combobulate)))
         (:activation-nodes
          ((:nodes ((exclude (all) ,@combobulate-erlang--wrappers)) :position at))
          :selector (:choose node :match-children t)))))))

(define-combobulate-language
 :name erlang
 :major-modes (erlang-ts-mode erlang-mode)
 :custom combobulate-erlang-definitions
 :setup-fn combobulate-erlang-setup)

(defconst combobulate-erlang--keyword-types
  '("case" "of" "receive" "if" "try" "catch" "after" "begin" "fun" "maybe" "else" "end")
  "Keyword tokens that delimit an Erlang construct.")

(defun combobulate-erlang--keywords (node)
  "Return the start positions of the keywords that delimit NODE.

The `after' of `try' and `receive' sits one level down, in its own
`try_after' or `receive_after' node."
  (when (member (treesit-node-type node)
                '("case_expr" "receive_expr" "if_expr" "try_expr" "block_expr"
                  "anonymous_fun" "maybe_expr"))
    (mapcan (lambda (child)
              (cond
               ((member (treesit-node-type child) '("try_after" "receive_after"))
                (list (treesit-node-start child)))
               ((and (not (treesit-node-check child 'named))
                     (member (treesit-node-type child) combobulate-erlang--keyword-types))
                (list (treesit-node-start child)))))
            (treesit-node-children node))))

(defun combobulate-erlang--sequence-target (direction)
  "Return the next keyword position in DIRECTION among the constructs around point."
  (let ((node (treesit-node-at (point) 'erlang))
        (target))
    (while (and node (not target))
      (let ((positions (combobulate-erlang--keywords node)))
        (setq target (if (eq direction 'next)
                         (seq-find (lambda (pos) (> pos (point))) positions)
                       (car (last (seq-filter (lambda (pos) (< pos (point))) positions))))))
      (setq node (treesit-node-parent node)))
    target))

(defun combobulate-erlang-navigate-sequence-next (&optional arg)
  "Move to the next keyword of the construct at point ARG times.

From `case' this visits `of', then `end'; from `try' also `catch'
and `after'."
  (interactive "^p")
  (dotimes (_ (or arg 1))
    (when-let* ((target (combobulate-erlang--sequence-target 'next)))
      (goto-char target))))

(defun combobulate-erlang-navigate-sequence-previous (&optional arg)
  "Move to the previous keyword of the construct at point ARG times."
  (interactive "^p")
  (dotimes (_ (or arg 1))
    (when-let* ((target (combobulate-erlang--sequence-target 'previous)))
      (goto-char target))))

(defun combobulate-erlang-setup (_)
  (let ((map (combobulate-read map)))
    (define-key map [remap combobulate-navigate-sequence-next]
                #'combobulate-erlang-navigate-sequence-next)
    (define-key map [remap combobulate-navigate-sequence-previous]
                #'combobulate-erlang-navigate-sequence-previous)
    (define-key map [remap combobulate-kill-node-dwim] #'combobulate-erlang-kill-node-dwim)
    (define-key map [remap combobulate-drag-up] #'combobulate-erlang-drag-up)
    (define-key map [remap combobulate-drag-down] #'combobulate-erlang-drag-down)
    (define-key map [remap combobulate-splice-up] #'combobulate-erlang-splice-up)
    (define-key map [remap combobulate-splice-down] #'combobulate-erlang-splice-down)
    (define-key map [remap combobulate-splice-self] #'combobulate-erlang-splice-self)
    (define-key map [remap combobulate-splice-parent] #'combobulate-erlang-splice-parent)))

;;; Editing

(defun combobulate-erlang--kind (node)
  "Return a string naming the kind of NODE for same-kind navigation.

Attributes such as `-doc' are grouped by name, and calls by the
function they call."
  (let ((field-text (lambda (n field)
                      (treesit-node-text (treesit-node-child-by-field-name n field) t))))
    (pcase (treesit-node-type node)
      ("wild_attribute" (treesit-node-text (treesit-node-child-by-field-name node "name") t))
      ("call" (concat "call " (funcall field-text node "expr")))
      ("remote" (concat "call " (funcall field-text node "module")
                        (funcall field-text (treesit-node-child-by-field-name node "fun") "expr")))
      (type type))))

(defun combobulate-erlang--same-kind-target (direction)
  "Return the nearest sibling in DIRECTION of the same kind as the one at point.

From a function clause, this is the first clause of the next or
previous function, so the other clauses of the current one are skipped."
  (with-navigation-nodes (:procedures (combobulate-read procedures-sibling))
    (let* ((real (lambda (node) (if (combobulate-proxy-node-p node)
                                    (combobulate-proxy-node-to-real-node node)
                                  node)))
           (nearest (save-excursion
                      (combobulate-skip-whitespace-forward t)
                      (combobulate--get-nearest-navigable-node)))
           (current (and nearest (funcall real (or (combobulate-nav-get-self-sibling nearest) nearest))))
           (kind (and current (combobulate-erlang--kind current)))
           (key (and current (combobulate-erlang--clause-key current)))
           (same (and kind
                      (seq-filter (lambda (node)
                                    (and (equal (combobulate-erlang--kind node) kind)
                                         (not (and key (equal (combobulate-erlang--clause-key node) key)))))
                                  (mapcar real (combobulate-nav-get-siblings current))))))
      (if (eq direction 'next)
          (seq-find (lambda (node) (> (treesit-node-start node) (treesit-node-start current))) same)
        (let ((target (car (last (seq-filter (lambda (node)
                                               (< (treesit-node-start node) (treesit-node-start current)))
                                             same)))))
          (when (and target key)
            (let ((target-key (combobulate-erlang--clause-key target))
                  (previous))
              (while (and (setq previous (treesit-node-prev-sibling target t))
                          (equal (combobulate-erlang--clause-key previous) target-key))
                (setq target previous))))
          target)))))

(defun combobulate-erlang-navigate-next-same-kind (&optional arg)
  "Move to the next sibling of the same kind ARG times.

From a function clause this reaches the next function; from `-spec'
the next `-spec'; from a call to `io:format' the next such call."
  (interactive "^p")
  (dotimes (_ (or arg 1))
    (combobulate-visual-move-to-node (combobulate-erlang--same-kind-target 'next))))

(defun combobulate-erlang-navigate-previous-same-kind (&optional arg)
  "Move to the previous sibling of the same kind ARG times."
  (interactive "^p")
  (dotimes (_ (or arg 1))
    (combobulate-visual-move-to-node (combobulate-erlang--same-kind-target 'previous))))

(defun combobulate-erlang--separator-p (node)
  (and node
       (not (treesit-node-check node 'named))
       (member (treesit-node-type node) '("," ";"))))

(defun combobulate-erlang--line-start-p (pos)
  (save-excursion
    (goto-char pos)
    (looking-back "^[ \t]*" (line-beginning-position))))

(defun combobulate-erlang--line-end-p (pos)
  (save-excursion
    (goto-char pos)
    (looking-at-p "[ \t]*$")))

(defun combobulate-erlang--whole-lines (start end)
  "Return START and END widened to whole lines.

Return nil if the region shares its lines with other code."
  (when (and (combobulate-erlang--line-start-p start) (combobulate-erlang--line-end-p end))
    (cons (save-excursion (goto-char start) (line-beginning-position))
          (save-excursion (goto-char end) (min (point-max) (1+ (line-end-position)))))))

(defun combobulate-erlang--kill-range (node)
  "Return the (START . END) region to delete when killing NODE.

The region takes one separator with it: the one after NODE, or the
one before it when NODE is the last element.  When NODE is on
lines of its own, the region covers those whole lines."
  (let ((start (treesit-node-start node))
        (end (treesit-node-end node))
        (next (treesit-node-next-sibling node))
        (prev (treesit-node-prev-sibling node)))
    (cond
     ((combobulate-erlang--separator-p next)
      (let ((after (treesit-node-next-sibling next)))
        (if (and after
                 (combobulate-erlang--line-start-p start)
                 (combobulate-erlang--line-start-p (treesit-node-start after)))
            (cons (save-excursion (goto-char start) (line-beginning-position))
                  (save-excursion (goto-char (treesit-node-start after)) (line-beginning-position)))
          (cons start (if after (treesit-node-start after) (treesit-node-end next))))))
     ((combobulate-erlang--separator-p prev)
      (let ((before (treesit-node-prev-sibling prev)))
        (cons (if before (treesit-node-end before) (treesit-node-start prev)) end)))
     (t (or (combobulate-erlang--whole-lines start end) (cons start end))))))

(defun combobulate-erlang--forms ()
  "Return the top-level forms, without comments."
  (seq-remove (lambda (node) (equal (treesit-node-type node) "comment"))
              (treesit-node-children (treesit-buffer-root-node 'erlang) t)))

(defun combobulate-erlang--clause-key (form)
  "Return the name and arity of the function that FORM is a clause of."
  (when (equal (treesit-node-type form) "fun_decl")
    (let ((clause (treesit-node-child-by-field-name form "clause")))
      (cons (treesit-node-text (treesit-node-child-by-field-name clause "name") t)
            (treesit-node-child-count (treesit-node-child-by-field-name clause "args") t)))))

(defun combobulate-erlang--fix-terminators (pos)
  "End each function clause near POS with `;', or `.' if it is the function's last.

Each clause of a function is its own form and carries its own
terminator, so moving or removing one can leave the wrong one."
  (let* ((forms (combobulate-erlang--forms))
         (index (or (seq-position forms pos (lambda (form pos) (<= pos (treesit-node-end form))))
                    (1- (length forms))))
         (edits))
    (cl-loop for i from (max 0 (- index 2)) to (min (1- (length forms)) (+ index 2))
             for form = (nth i forms)
             for key = (combobulate-erlang--clause-key form)
             when key
             do (let* ((terminator (treesit-node-child form -1))
                       (wanted (if (equal key (combobulate-erlang--clause-key (nth (1+ i) forms))) ";" ".")))
                  (unless (or (treesit-node-check terminator 'named)
                              (equal (treesit-node-text terminator t) wanted))
                    (push (cons (treesit-node-start terminator) wanted) edits))))
    (save-excursion
      (pcase-dolist (`(,start . ,text) edits)
        (goto-char start)
        (delete-char 1)
        (insert text)))))

(defun combobulate-erlang--top-level-p (node)
  (equal (treesit-node-type (treesit-node-parent node)) "source_file"))

(defun combobulate-erlang--form-at-point ()
  "Return the top-level form that starts at point, after any whitespace."
  (let ((pos (save-excursion (skip-chars-forward " \t\n") (point))))
    (seq-find (lambda (form) (= (treesit-node-start form) pos)) (combobulate-erlang--forms))))

(defun combobulate-erlang-kill-node-dwim (&optional arg)
  "Like `combobulate-kill-node-dwim', but keep the code valid.

Takes one `,' or `;' along with the node, and fixes the `;' and `.'
of the surrounding function clauses when killing a clause."
  (interactive "p")
  (dotimes (_ (or arg 1))
    (with-navigation-nodes (:procedures (combobulate-read procedures-sibling))
      (when-let* ((nearest (save-excursion
                             (combobulate-skip-whitespace-forward t)
                             (combobulate--get-nearest-navigable-node)))
                  (sibling (or (combobulate-nav-get-self-sibling nearest) nearest))
                  (node (if (combobulate-proxy-node-p sibling)
                            (combobulate-proxy-node-to-real-node sibling)
                          sibling)))
        (unless (combobulate-node-on-or-after-point-p node)
          (error "No node to kill"))
        (let* ((top-level (combobulate-erlang--top-level-p node))
               (range (if top-level
                          (or (combobulate-erlang--whole-lines (treesit-node-start node)
                                                               (treesit-node-end node))
                              (cons (treesit-node-start node) (treesit-node-end node)))
                        (combobulate-erlang--kill-range node)))
               (text (treesit-node-text node t))
               (description (combobulate-pretty-print-node node)))
          (delete-region (car range) (cdr range))
          (when top-level
            (combobulate-erlang--fix-terminators (car range)))
          (if (memq last-command '(combobulate-kill-node-dwim combobulate-erlang-kill-node-dwim))
              (kill-append text nil)
            (kill-new text))
          (combobulate-message (concat "Killed " description)))))))

(defun combobulate-erlang--drag-command (command arg direction)
  "Run the drag COMMAND with ARG, then fix moved function clause terminators.

Refuse to swap two clauses of different kinds, such as a `receive'
clause with its `after', which would move code into the wrong section."
  (let* ((pos (save-excursion (skip-chars-forward " \t") (point)))
         (clause (seq-find (lambda (node)
                             (and (member (treesit-node-type node) combobulate-erlang--clause-types)
                                  (= (treesit-node-start node) pos)))
                           (combobulate-all-nodes-at-point)))
         (neighbor (and clause (if (eq direction 'down)
                                   (treesit-node-next-sibling clause t)
                                 (treesit-node-prev-sibling clause t)))))
    (when (and neighbor (not (equal (treesit-node-type neighbor) (treesit-node-type clause))))
      (user-error "Cannot move a clause across `catch' or `after'")))
  (let ((top-level (combobulate-erlang--form-at-point)))
    (funcall command arg)
    (when top-level
      (combobulate-erlang--fix-terminators (point)))))

(defun combobulate-erlang-drag-up (&optional arg)
  "Like `combobulate-drag-up', but keep function clause terminators valid."
  (interactive "^p")
  (combobulate-erlang--drag-command #'combobulate-drag-up arg 'up))

(defun combobulate-erlang-drag-down (&optional arg)
  "Like `combobulate-drag-down', but keep function clause terminators valid."
  (interactive "^p")
  (combobulate-erlang--drag-command #'combobulate-drag-down arg 'down))

(defun combobulate-erlang--statement-at-point ()
  "Return the body statement at point and the node whose statements it is among."
  (let ((node (treesit-node-at (save-excursion (skip-chars-forward " \t\n") (point)) 'erlang)))
    (while (and node
                (not (member (treesit-node-type (treesit-node-parent node))
                             '("clause_body" "block_expr" "try_after" "try_expr"))))
      (setq node (treesit-node-parent node)))
    (when (and node (not (member (treesit-node-type node) combobulate-erlang--clause-types))
               (treesit-node-check node 'named))
      (cons node (treesit-node-parent node)))))

(defun combobulate-erlang--splice-target (container)
  "Return the construct that splicing the statements of CONTAINER replaces."
  (pcase (treesit-node-type container)
    ((or "block_expr" "try_expr") container)
    ("try_after" (treesit-node-parent container))
    ("clause_body"
     (let ((clause (treesit-node-parent container)))
       (unless (member (treesit-node-type clause) '("function_clause" "fun_clause"))
         (treesit-node-parent clause))))))

(defun combobulate-erlang--splice (partitions)
  "Replace the construct around point with some of the statements at point.

PARTITIONS says which statements to keep, relative to the one at
point: `self', `before' and `after'."
  (pcase-let* ((`(,statement . ,container)
                (or (combobulate-erlang--statement-at-point)
                    (user-error "Nothing to splice here")))
               (target (or (combobulate-erlang--splice-target container)
                           (user-error "Cannot splice statements out of a function")))
               (statements (seq-filter (lambda (child)
                                         (and (equal (treesit-node-field-name child) "exprs")
                                              (treesit-node-check child 'named)))
                                       (treesit-node-children container)))
               (index (seq-position statements statement #'treesit-node-eq))
               (kept (seq-keep (lambda (i)
                                 (and (cond ((< i index) (memq 'before partitions))
                                            ((> i index) (memq 'after partitions))
                                            (t (memq 'self partitions)))
                                      (nth i statements)))
                               (number-sequence 0 (1- (length statements)))))
               (text (buffer-substring (treesit-node-start (car kept))
                                       (treesit-node-end (car (last kept)))))
               (shift (- (save-excursion (goto-char (treesit-node-start target)) (current-column))
                         (save-excursion (goto-char (treesit-node-start (car kept))) (current-column))))
               (start (treesit-node-start target)))
    (delete-region start (treesit-node-end target))
    (goto-char start)
    (insert text)
    (indent-rigidly (save-excursion (goto-char start) (line-beginning-position 2)) (point) shift)
    (goto-char start)
    (combobulate-message (format "Spliced %d of %d statements" (length kept) (length statements)))))

(defun combobulate-erlang-splice-up (&optional _arg)
  "Replace the construct around point with this statement and the ones after it."
  (interactive "^p")
  (combobulate-erlang--splice '(self after)))

(defun combobulate-erlang-splice-down (&optional _arg)
  "Replace the construct around point with this statement and the ones before it."
  (interactive "^p")
  (combobulate-erlang--splice '(self before)))

(defun combobulate-erlang-splice-self (&optional _arg)
  "Replace the construct around point with the statement at point."
  (interactive "^p")
  (combobulate-erlang--splice '(self)))

(defun combobulate-erlang-splice-parent (&optional _arg)
  "Replace the construct around point with all the statements of this body."
  (interactive "^p")
  (combobulate-erlang--splice '(before self after)))

(provide 'combobulate-erlang)
;;; combobulate-erlang.el ends here
