;;; combobulate-manipulation.el --- manipulate structured text with combobulate  -*- lexical-binding: t; -*-

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

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-misc)
(require 'combobulate-interface)
(require 'tempo)
(require 'map)
;;; for python-specific indent stuff
(require 'python)


(declare-function combobulate--mc-edit-nodes "combobulate-contrib")

(defun combobulate--refactor-insert-copied-values (values)
  (let ((start-marker (point-marker)) (after-marker) (pt))
    ;; move this into a distinct function that can reproduce stored
    ;; input.
    (goto-char (point-marker))
    ;; (forward-line 0)
    (skip-syntax-backward " ")
    (setq pt (point))
    (let ((ct 0))
      (dolist (s values)
        (unless (string-blank-p s)
          (save-excursion
            (insert s)
            (setq pt (point)))
          (cl-incf ct)
          (when (= ct 1)
            (save-excursion
              (back-to-indentation)
              (setq start-marker (point-marker))))
          (goto-char pt))))
    (back-to-indentation)
    (delete-blank-lines)
    (setq after-marker (point-marker))
    (when combobulate-manipulation-indent-after-edit
      (indent-region start-marker (save-excursion
                                    (goto-char after-marker)
                                    (end-of-line)
                                    (point))))
    (goto-char start-marker)))

(defun combobulate--refactor-get-all-overlays ()
  (seq-filter
   (lambda (ov)
     (overlay-get ov 'combobulate-refactor-actions))
   (car (overlay-lists))))

(defun combobulate--refactor-clear-overlays (overlays)
  (mapc #'delete-overlay overlays))

;; (defun combobulate--refactor-divide-overlays (keep-overlays)
;;   (let ((markers))
;;     (dolist (ov-k keep-overlays)
;;       (push ov-k markers)
;;       (when (eq (overlay-get ov-k 'combobulate-refactor-actions) 'copy-region)
;;         (dolist (overlapped-ov (overlays-in (overlay-start ov-k)
;;                                             (overlay-end ov-k)))
;;           (when-let (overlapped-ov-action (overlay-get overlapped-ov 'combobulate-refactor-actions))
;;             (if (eq overlapped-ov-action 'delete-region)
;;                 (let ((start-overlaps (in-range (overlay-start overlapped-ov)
;;                                                 (overlay-start ov-k)
;;                                                 (overlay-end ov-k)))
;;                       (new-ov))
;;                   (when start-overlaps
;;                     (setq new-ov (copy-overlay overlapped-ov))
;;                     (move-overlay new-ov
;;                                   (overlay-start new-ov)
;;                                   (overlay-start ov-k))
;;                     (move-overlay overlapped-ov
;;                                   (if start-overlaps
;;                                       (overlay-end ov-k)
;;                                     (overlay-start overlapped-ov))
;;                                   (overlay-end overlapped-ov))
;;                     (push new-ov markers)))
;;               (push overlapped-ov markers))))))
;;     markers))

(defvar combobulate-refactor--copied-values nil)

(defmacro combobulate-refactor (&rest body)
  (declare (indent 1) (debug (form body)))
  (let ((--markers (gensym)))
    `(let ((,--markers nil))
       (cl-flet* ((add-marker (ov &optional end)
                    (if end
                        (setq ,--markers (cons ov ,--markers))
                      (push ov ,--markers))
                    ov)
                  ;; (ordered-apply (&rest markers)
                  ;;   (add-marker
                  ;;    (let* ((ov-start (overlay-start (car markers)))
                  ;;           (ov-end (overlay-end (car markers)))
                  ;;           (new-ov (make-overlay ov-start ov-end))
                  ;;           (n (seq-uniq (apply #'append
                  ;;                               (mapcar (lambda (o) (overlay-get o 'combobulate-refactor-actions))
                  ;;                                       markers)))))
                  ;;      (overlay-put new-ov 'combobulate-refactor-actions n)
                  ;;      (mapc #'delete-overlay markers)
                  ;;      new-ov)))
                  (mark-range-deleted (beg end)
                    (add-marker (combobulate--refactor-mark-deleted beg end)))
                  (mark-copy (start end)
                    (add-marker (combobulate--refactor-mark-copy start end 'combobulate-refactor--copied-values)))
                  (mark-node-copy (n)
                    (mark-copy (combobulate-node-start n) (combobulate-node-end n)))
                  (mark-node-indent (beg end target-pt baseline-column)
                    (add-marker (combobulate--refactor-mark-indent beg end target-pt baseline-column)))
                  (mark-node-deleted (n)
                    (add-marker (combobulate--refactor-mark-deleted (combobulate-node-start n)
                                                                    (combobulate-node-end n))))
                  (mark-node-highlighted (n &optional advance)
                    (add-marker (combobulate--refactor-mark-highlighted (combobulate-node-start n)
                                                                        (combobulate-node-end n)
                                                                        advance)))
                  (mark-point (&optional pt)
                    (add-marker (combobulate--refactor-mark-position (or pt (point)))))
                  (rollback ()
                    (mapc (lambda (ov) (delete-overlay ov))
                          ,--markers)
                    (setq ,--markers nil)
                    (combobulate-reinitialize-parser))
                  (commit ()
                    (setq combobulate-refactor--copied-values nil)
                    (mapc (lambda (ov) (combobulate--refactor-commit ov t))
                          (seq-sort (lambda (a b) (> (overlay-start a)
                                                (overlay-end b)))
                                    ,--markers))
                    ;; clean up.
                    (setq ,--markers nil)
                    (combobulate-reinitialize-parser)))
         (prog1 (atomic-change-group
                  ,@body)
           (when ,--markers
             (error "Uncommitted changes: %s"
                    (prog1 ,--markers
                      (combobulate--refactor-clear-overlays
                       (combobulate--refactor-get-all-overlays))))))))))

(defun combobulate-procedure-get-activation-nodes (procedures)
  "Given a list of PROCEDURES, return a merged list of all activation nodes."
  (flatten-tree (mapcar (lambda (procedure)
                          (mapcar (lambda (activation-node)
                                    (let ((node (plist-get activation-node :node)))
                                      (if (consp node) node (cons node nil))))
                                  (plist-get procedure :activation-nodes)))
                        procedures)))

(defun combobulate-procedure-get-parent-nodes (point-node possible-parents)
  "Find POSSIBLE-PARENTS on or near POINT-NODE.

POSSIBLE-PARENTS must be a list of strings or forms.

If a form, it must be a valid combobulate query that contains one or more
`@parent' labels.

Returns a list of parents ordered closest to farthest."
  (let* ((actual-parents (combobulate-get-parents point-node))
         (matched-parents))
    (seq-uniq
     (seq-sort #'combobulate-node-after-node-p
               (dolist (possible-parent possible-parents matched-parents)
                 (cond
                  ((stringp possible-parent)
                   (when-let (m (seq-find (lambda (p) (equal (combobulate-node-type p) possible-parent))
                                          actual-parents))
                     (push m matched-parents)))
                  ((consp possible-parent)
                   (dolist (actual-parent actual-parents)
                     (pcase-dolist (`(,label . ,match) (combobulate-query-search actual-parent possible-parent t))
                       (when (eq label '@parent)
                         (push match matched-parents)))))))))))


(defun combobulate-procedure-find-applicable-procedures (point-node procedures)
  "Check if PROCEDURES apply to NODE and return the ones that do."
  (cl-flet
      ((node-match (n pos)
         (and (equal (combobulate-node-type point-node) n)
              (cond
               ((eq pos 'at-or-in)
                (or (combobulate-point-at-beginning-of-node-p point-node)
                    (combobulate-point-in-node-range-p point-node)))
               ((eq pos 'in)
                (combobulate-point-in-node-range-p point-node)
                ;; `combobulate-point-in-node-range-p' is t if
                ;; we're at the start of the node, but that is
                ;; not allowed here.
                (not (combobulate-point-at-beginning-of-node-p point-node)))
               ((eq pos 'at)
                (combobulate-point-at-beginning-of-node-p point-node))
               (t (error "Unknown `:position' specifier `%s'" pos))))))
    (let ((matches))
      (dolist (procedure procedures)
        (dolist (activation-node (plist-get procedure :activation-nodes))
          (when-let (result (map-let (:node :find-parent :find-immediate-parent :find-base-rule-parent :position) activation-node
                              (when-let (match-node (pcase node
                                                      ((and (pred stringp) node)
                                                       (node-match node position))
                                                      ((and (pred consp) node-list)
                                                       (seq-find (lambda (n) (node-match n position)) node-list))))
                                (cond
                                 (find-base-rule-parent
                                  (car-safe
                                   (if (booleanp find-base-rule-parent)
                                       (combobulate-find-similar-ancestors
                                        point-node
                                        (combobulate-get-parents point-node))
                                     (map-let (:keep-types :remove-types) find-base-rule-parent
                                       (combobulate-filter-nodes
                                        (combobulate-find-similar-ancestors
                                         point-node
                                         (combobulate-get-parents point-node))
                                        :keep-types keep-types
                                        :remove-types remove-types)))))
                                 (find-parent
                                  (car-safe (combobulate-procedure-get-parent-nodes
                                             point-node
                                             (if (stringp find-parent)
                                                 (list find-parent)
                                               find-parent))))
                                 (find-immediate-parent
                                  (if (booleanp find-immediate-parent)
                                      (car-safe (combobulate-get-parents point-node))
                                    (and (member (combobulate-node-type (car-safe (combobulate-get-parents point-node)))
                                                 find-immediate-parent)
                                         (car-safe (combobulate-get-parents point-node)))))
                                 (t point-node)))))
            (push (cons result procedure) matches))))
      matches)))

(defun combobulate-procedure-apply-procedure (point-node parent-node procedure)
  "Apply PROCEDURE to PARENT-NODE via POINT-NODE and return the matches.

The PROCEDURE should have either `:match-siblings',
`:match-children' or `:match-query'.

If `:match-query' is used, any node labelled `@ignore' is
filtered from the list of returned matches."
  (let ((matches) (value))
    (dolist (key '(:match-siblings :match-children :match-query :match-dynamic-query))
      (when (setq value (plist-get procedure key))
        (setq matches (append
                       matches
                       (pcase key
                         (:match-siblings
                          (map-let (:keep-parent :keep-types :remove-types) value
                            (append (list (cons (if keep-parent '@keep '@discard) parent-node))
                                    (mapcar (lambda (n) (cons '@keep n))
                                            (or (combobulate-filter-nodes
                                                 (combobulate-get-siblings-of-node point-node nil)
                                                 :keep-types keep-types
                                                 :remove-types remove-types)
                                                (list point-node))))))
                         (:match-children
                          (mapcar (lambda (n) (cons '@keep n))
                                  (if (and (booleanp value) value)
                                      (combobulate-get-children (if (equal parent-node point-node) point-node parent-node) :all-nodes t)
                                    (map-let (:anonymous :excluded-fields :included-fields
                                                         :remove-types :keep-types :all
                                                         :all-nodes)
                                        value
                                      (combobulate-get-children
                                       (if (equal parent-node point-node) point-node parent-node)
                                       :anonymous anonymous :excluded-fields excluded-fields :included-fields included-fields
                                       :all all :remove-types remove-types :keep-types keep-types :all-nodes all-nodes)))))
                         ((and (or :match-dynamic-query :match-query) query-type)
                          (seq-remove
                           (lambda (n)
                             (and (eq (car n) 'ignore)))
                           (combobulate-query-search
                            parent-node
                            (if (eq query-type :match-dynamic-query)
                                (combobulate-query-build-nested-query
                                 (append ;; (list parent-node)
                                  (seq-take-while (lambda (p) (not (equal p parent-node)))
                                                  (combobulate-get-parents point-node)))
                                 value)
                              value)
                            t nil nil)))
                         (_ value))))))
    ;; final post-processing
    (map-let (:keep-types :remove-types) procedure
      (combobulate-filter-nodes
       matches
       :keep-types keep-types
       :remove-types remove-types
       :testfn #'cdr))))


(defun combobulate-procedure-start (point-node &optional procedures)
  "Attempt an edit procedure at POINT-NODES and optionally with PROCEDURES.

If PROCEDURES is nil, then default to
`combobulate-manipulation-default-procedures', which is
frequently set in `with-navigation-nodes'.

This function raises an ambiguity error if there is more than one
procedure in PROCEDURES that matches POINT-NODE. If there are no
matches, another error is raised. If there is exactly one match
then a cons cell of this shape is returned:

   (MATCHED-PROCEDURE . (MATCHED-PARENT-NODE . ((@label . match-node-1) ... )))"
  (setq procedures (or procedures combobulate-manipulation-default-procedures))
  (let ((procedures (combobulate-procedure-find-applicable-procedures point-node procedures))
        (procedure))
    (cond
     ((> (length procedures) 1)
      (error "Ambiguous edit. `%s' matches multiple activation nodes: `%s'"
             (combobulate-pretty-print-node point-node) procedures))
     ((null procedures)
      (error "There are no valid procedures that apply to `%s'."
             (combobulate-pretty-print-node point-node)))
     (t
      (setq procedure (pop procedures))
      ;; (unless (equal (car procedure) point-node)
      ;;   (error "`%s' does not equal point node %s" (car procedure) point-node))
      (cons procedure (combobulate-procedure-apply-procedure point-node (car procedure) (cdr procedure)))))))

(defun combobulate-procedure-start-aggressive (&optional pt procedures)
  "Attempt to find a procedure at PT."
  (catch 'found
    (let ((procedure))
      (dolist (node (save-excursion (goto-char (or pt (point)))
                                    (reverse (combobulate-all-nodes-at-point))))
        (setq procedure (ignore-errors (combobulate-procedure-start node procedures)))
        (when procedure
          (throw 'found procedure))))))

(defun combobulate-tally-nodes (nodes)
  "Groups NODES into labels (if any) and their types and tallies them.

If NODES is a list of `(@label . node)' cons cells, tally nodes by that
first; followed by the node type of each grouped label."
  (when nodes
    (if (and (consp nodes) (consp (car nodes)))
        (mapconcat
         (lambda (g) (pcase-let ((`(,label . ,rest) g))
                  (concat (capitalize (substring (symbol-name label) 1))
                          " "
                          (combobulate-tally-nodes (mapcar 'cdr rest)))))
         (combobulate-group-nodes nodes #'car) ". ")
      (string-join (mapcar (lambda (group)
                             (concat
                              (propertize (int-to-string (length (cdr group)))
                                          'face 'combobulate-active-indicator-face)
                              " "
                              (format "`%s'" (car group))))
                           (combobulate-group-nodes nodes #'combobulate-pretty-print-node-type))
                   "; "))))

(defun combobulate-edit-cluster (node &optional point-at-end)
  "Edit CLUSTER of nodes at, or around, NODE.

If POINT-AT-END is non-nil, then point is placed at the end of
the node boundary of each match instead of the beginning."
  (let ((matches))
    (pcase-let ((`((,parent-node . ,_) . ,labelled-matches) (combobulate-procedure-start node)))
      ;; Remove `@discard' matches.
      (setq matches (mapcar 'cdr (seq-remove (lambda (m) (equal (car m) '@discard))
                                             labelled-matches)))
      (combobulate--mc-edit-nodes matches point-at-end)
      (cond ((= (length matches) 0)
             (combobulate-message "There are zero nodes available to edit."))
            (t (combobulate-message
                (concat "Editing "
                        (combobulate-tally-nodes matches)
                        " in "  (propertize
                                 (combobulate-node-type parent-node)
                                 'face 'combobulate-active-indicator-face))))))))

(defun combobulate-vanish-node (&optional arg)
  "Vanishes the node at point and attempts to preserve its children."
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:nodes
                            (combobulate-procedure-get-activation-nodes
                             combobulate-manipulation-splicing-procedures)
                            :procedures combobulate-manipulation-splicing-procedures)
      (combobulate-splice (combobulate--get-nearest-navigable-node)
                          '(before after around self)))))

(defun combobulate--vanish-node (node)
  "Vanish NODE, keeping all its children."
  (if-let* ((q (cdr (assoc (combobulate-node-type node) combobulate-manipulation-splicing-procedures)))
            (query (list q))
            (node-extent (combobulate-node-range-extent
                          (combobulate--query-from-node query node nil nil t)))
            (text (apply #'buffer-substring-no-properties node-extent)))
      (combobulate--replace-node node text)
    (error "Cannot vanish node `%s'" (combobulate-pretty-print-node node))))

(defun combobulate--swap-node-regions (node-a node-b)
  "Swaps the region substring in NODE-A with NODE-B"
  (transpose-subr-1 (combobulate-node-range node-a) (combobulate-node-range node-b)))

(defun combobulate-transpose-sexps-1 (backward)
  "Capture a transposable node, either forward or BACKWARD."
  ;; do not set `:nodes' here to allow this function to work with any
  ;; node type group set in the caller.
  (with-navigation-nodes (:nodes combobulate-navigation-sexp-nodes
                                 :backward backward :skip-prefix t)
    (combobulate-forward-sexp-function-1 backward)))

(defun combobulate-transpose-sexps ()
  "Transpose sexp-like nodes around point.

If there are no legitimate sexp nodes around point, fall back to
\\[transpose-sexp]."
  (interactive)
  ;; this covers the most common case where two nodes are "adjacent"
  ;; to the point.
  (if-let ((backward-node (combobulate-transpose-sexps-1 t))
           (forward-node (combobulate-transpose-sexps-1 nil)))
      (progn
        (combobulate--goto-node forward-node t)
        (save-excursion
          (combobulate--swap-node-regions backward-node forward-node)))
    ;; for everything else, fall back to the normal (but augmented,
    ;; because `combobulate-forward-sexp-function' is used by
    ;; `transpose-sexps') to attempt a transposition by shifting
    ;; point around.
    (transpose-sexps 1 t)))

(defun combobulate-transpose-sexp-function (arg)
  ;; Here we should try to simulate the behavior of
  ;; (cons (progn (forward-sexp x) (point))
  ;;       (progn (forward-sexp (- x)) (point)))
  ;; Except that we don't want to rely on the second forward-sexp
  ;; putting us back to where we want to be, since forward-sexp-function
  ;; might do funny things like infix-precedence.
  (if (if (> arg 0)
	  (looking-at "\\sw\\|\\s_")
	(and (not (bobp))
	     (save-excursion
               (forward-char -1)
               (looking-at "\\sw\\|\\s_"))))
      ;; Jumping over a symbol.  We might be inside it, mind you.
      (progn (funcall (if (> arg 0)
			  #'skip-syntax-backward #'skip-syntax-forward)
		      "w_")
	     (cons (save-excursion (forward-sexp arg) (point)) (point)))
    ;; Otherwise, we're between sexps.  Take a step back before jumping
    ;; to make sure we'll obey the same precedence no matter which
    ;; direction we're going.
    (funcall (if (> arg 0) #'skip-syntax-backward #'skip-syntax-forward)
             " .")
    (cons (save-excursion (forward-sexp arg) (point))
	  (progn (while (or (forward-comment (if (> arg 0) 1 -1))
			    (not (zerop (funcall (if (> arg 0)
						     #'skip-syntax-forward
						   #'skip-syntax-backward)
						 ".")))))
		 (point)))))

(defun combobulate-edit-cluster-dwim (arg)
  "Edit clusters of nodes around point.

This looks for clusters of nodes to edit in
`combobulate-navigation-editable-nodes'.

If you specify a prefix ARG, then the points are placed at the
end of each edited node."
  (interactive "P")
  (with-navigation-nodes (:nodes combobulate-navigation-editable-nodes
                                 :procedures combobulate-manipulation-edit-procedures)
    (if-let ((node (combobulate--get-nearest-navigable-node)))
        (combobulate-edit-cluster node arg)
      (error "Cannot find any editable clusters here"))))

(defun combobulate--kill-node (node)
  "Kill NODE in the current buffer."
  (and node (kill-region (combobulate-node-start node)
                         (combobulate-node-end node)))
  (combobulate-delete-whitespace))

(defun combobulate--kill-nodes (nodes)
  "Kill between the smallest and greatest range of NODES."
  (apply #'kill-region (combobulate-node-range-extent nodes)))

(defun combobulate--mark-extent (start end &optional swap beginning-of-line)
  "Set mark to START and point to END and activates the mark.

If SWAP is non-nil, the point and mark is exchanged after, thus
reversing the order of point and mark.

If BEGINNING-OF-LINE is non-nil, then the mark is set to the
beginning of line instead of START, but only if the text is
considered whitespace by the mode's syntax table."
  (cl-macrolet
      ((change-position (f)
         `(save-excursion
            (goto-char ,f)
            (skip-chars-backward combobulate-skip-prefix-regexp
                                 (line-beginning-position))
            (when (bolp)
              (setf ,f (point))))))
    (when beginning-of-line
      (change-position start)
      (change-position end))
    (push-mark start t)
    (goto-char end)
    (activate-mark)
    (when swap
      (exchange-point-and-mark))
    (cons start end)))

(defun combobulate-extend-region-to-whole-lines (start end)
  "Alter the region between START and END but made up of whole lines if possible."
  (dolist (pos (list start end))
    (save-excursion
      (goto-char pos)
      (skip-chars-backward combobulate-skip-prefix-regexp
                           (line-beginning-position))
      (when (bolp)
        (setq start (point)))))
  (list start end))

(defun combobulate-node-region-lines (node)
  "Return NODE's region but made up of whole lines if possible."
  (combobulate-extend-region-to-whole-lines (combobulate-node-start node)
                                            (combobulate-node-end node)))



(defun combobulate-indent-string-first-line (text target-col)
  "Corrects the indentation of the first line of TEXT to TARGET-COL."
  (let ((lines (split-string text "\n")))
    (string-join
     (cons (combobulate-indent-string (car lines) target-col)
           (cdr lines))
     "\n")))

(defun combobulate-indent-string (text target-col &optional absolute)
  ""
  (let* ((lines (split-string text "\n")))
    ;; calculate the indentation at the beginning of the line, if
    ;; any.
    (string-join
     (mapcar (lambda (line)
               (if absolute
                   (error "Absolute indentation not implemented yet.")
                 (if (> target-col 0)
                     (concat (make-string target-col ? ) line)
                   (string-trim-left line (rx-to-string `(** 0 ,(abs target-col) space))))))
             lines)
     "\n")))

(defun combobulate--clone-node (node position)
  "Clone NODE and place it at POSITION."
  (let* ((col (save-excursion (goto-char position)
                              (current-indentation)))
         (node-col (save-excursion
                     (combobulate--goto-node node)
                     (current-indentation)))
         (node-text (combobulate-node-text node)))
    (goto-char position)
    (if (string-blank-p (buffer-substring-no-properties (save-excursion
                                                          (combobulate--goto-node node)
                                                          (beginning-of-line)
                                                          (point))
                                                        (combobulate-node-start node)))
        (progn (split-line 1)
               (combobulate--refactor-insert-copied-values
                (list (combobulate-indent-string
                       ;; the first line may not include all of its
                       ;; indentation because the node extents won't
                       ;; include it. This fixes it so it does.
                       (combobulate-indent-string-first-line
                        node-text
                        node-col)
                       (- col node-col)))))
      (save-excursion
        (insert node-text)
        (unless (looking-at "\\s-")
          (insert " "))))))


(defun combobulate--mark-node (node &optional swap beginning-of-line)
  "Mark NODE in the current buffer.

See `combobulate--mark-extent' for argument explanations."
  (when node
    (combobulate--mark-extent (combobulate-node-start node)
                              (combobulate-node-end node)
                              swap
                              beginning-of-line)))

(defun combobulate--delete-node (node)
  "Deletes NODE in the current buffer."
  (and node (delete-and-extract-region (combobulate-node-start node)
                                       (combobulate-node-end node))))

(defun combobulate--replace-node (node text &optional before)
  "Replace NODE with TEXT and maybe place point BEFORE.

The NODE is deleted (`delete-region') and TEXT inserted in its place.

If BEFORE then point is placed before the text and not after.

The variable `combobulate-manipulation-indent-method' determines
how indentation of the TEXT happens.  This is mostly of interest
in Python where whitespace is very important, and automatic
indentation unreliable."
  (when node
    (atomic-change-group
      (let ((node-start (combobulate-node-start node)))
        (combobulate--goto-node node)
        (combobulate--delete-node node)
        (let ((beg) (end)
              (pt (point))
              (offset 0))
          (setq beg (pos-bol))
          (save-excursion (beginning-of-line) (insert text))
          (setq end (pos-eol))
          (goto-char node-start)
          (cond ((eq 'mode combobulate-manipulation-indent-method)
                 (indent-region beg end))
                ((eq 'first combobulate-manipulation-indent-method)
                 (setq offset (save-excursion
                                (goto-char pt)
                                (skip-syntax-forward " ")
                                (current-column)))
                 (indent-rigidly beg end (- (current-column) offset))))
          (when before
            (goto-char pt)))))
    (combobulate-delete-empty-lines)
    (combobulate-delete-whitespace)
    (when (eq 'mode combobulate-manipulation-indent-method)
      (indent-according-to-mode))))

(defun combobulate-kill-node-dwim (&optional arg)
  "Kill the most likely node on or near point ARG times.

The exact node that is killed will depend on the location of
point relative to the nodes in
`combobulate-navigation-default-nodes'."
  (interactive "p")
  (with-argument-repetition arg
    (when-let ((node (combobulate--get-nearest-navigable-node)))
      (combobulate-message "Killed" node)
      (combobulate--kill-node node))))

(defvar combobulate--tempo-point-pos nil
  "Position of where point must go after a tempo template is expanded.")

(defvar combobulate--tempo-indentation nil
  "Column offset (or current indentation) at a given place in a tempo template.")

(defun combobulate--tempo-handle-element-C (element)
  "Tempo ELEMENT `C'  stores the current indentation."
  (when (eq element 'C)
    (setq combobulate--tempo-indentation (current-indentation))
    ;; do this because tempo is buggy AF
    ""))

(defun combobulate--tempo-handle-element-c (element)
  "Tempo ELEMENT `c' goes to the stored indentation."
  (when (eq element 'c)
    (indent-to combobulate--tempo-indentation)
    ;; do this because tempo is buggy AF
    ""))

(defun combobulate--tempo-handle-element-@ (element)
  "Tempo ELEMENT `@' that remembers the position of point."
  (when (eq element '@)
    (setq combobulate--tempo-point-pos (point))
    ;; do this because tempo is buggy AF
    ""))

(defun combobulate--tempo-handle-element-y> (element)
  "Tempo ELEMENT `y>' that used hacky code to indent a region."
  (when (eq element 'y>)
    (progn
      (python-indent-shift-right (point) (mark) 4)
      (indent-line-to (car (last (python-indent-calculate-levels))))
      (exchange-point-and-mark))
    ;; do this because tempo is buggy AF
    ""))

(defvar combobulate-proffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-g" 'cancel)
    (define-key map "\M-c" 'recursive-edit)
    (define-key map [return] 'done)
    (define-key map [tab] 'next)
    (define-key map [S-iso-lefttab] 'prev)
    map)
  "Keymap for `combobulate-proffer-choices'.")

(cl-defun combobulate-proffer-choices (nodes action-fn &key (first-choice nil)
                                             (reset-point-on-abort t) (reset-point-on-accept nil)
                                             (highlight-first t) (extra-map nil)
                                             (flash-node nil) (unique-only t))
  "Interactively browse NODES one at a time with ACTION-FN applied to it.

Interactively, a user is expected to choose a node in NODES or
abandon the selection process altogether (see
`combobulate-proffer-map'.) If there is exactly one node in
NODES, then it is automatically selected.

ACTION-FN must be a function that takes four arguments:

   (CURRENT-NODE MARK-HIGHLIGHTED-FN MOVE-TO-NODE-FN MARK-DELETED-FN)

Where CURRENT-NODE is the proffered node out of NODES.  The three
additional arguments are optional; callers must optionally
`funcall' each one in the order that best makes sense for the
ACTION-FN.

MARK-HIGHLIGHTED-FN instructs `combobulate-refactor' to highlight
CURRENT-NODE. MOVE-TO-NODE-FN moves point to the beginning of
CURRENT-NODE. And MARK-DELETED-FN tells `combobulate-refactor' to
mark the node for deletion if a successful `commit' call takes
place.

The caller is responsible for invoking these functions in the
correct order (or not at all, if they are not required.)

Setting `:first-choice' to t prevents the system from proffering
choices at all; instead, the first choice is automatically picked.

When `:reset-point' is t, the point is reset to where it
was when the proffer was first started. If nil, the point is not
altered at all.

If `:flash-node' is t, then display a node tree in the echo area
alongside the status message.

If t, `:unique-only' filters out duplicate nodes *and*
nodes that share the same range extent. I.e., a `block' and a
`statement' node that effectively encompass the same range in the
buffer.

`:extra-map' is a list of cons cells consisting of (KEY . COMMAND)."
  (let ((proxy-nodes
         (and nodes (combobulate-make-proxy
                     ;; strip out duplicate nodes. That
                     ;; includes nodes that are duplicates
                     ;; of one another; however, we also
                     ;; strip out nodes that share the
                     ;; same range extent.
                     (if unique-only
                         (seq-uniq nodes (lambda (a b) (or (equal a b)
                                                      (equal (combobulate-node-range a)
                                                             (combobulate-node-range b)))))
                       nodes))))
        (result) (state 'continue) (current-node)
        (index 0) (pt (point))
        (map (if extra-map
                 (let ((map (make-sparse-keymap)))
                   (set-keymap-parent map combobulate-proffer-map)
                   (mapc (lambda (k) (define-key map (car k) (cdr k))) extra-map)
                   map)
               combobulate-proffer-map)))
    (if proxy-nodes
        (condition-case nil
            (with-undo-amalgamate
              (catch 'exit
                (while (eq state 'continue)
                  (catch 'next
                    (setq current-node (nth index proxy-nodes))
                    (combobulate-refactor
                        (funcall action-fn current-node
                                 (apply-partially #'mark-node-highlighted current-node)
                                 (apply-partially #'combobulate-move-to-node current-node)
                                 (apply-partially #'mark-node-deleted current-node))
                      (if (or (= (length proxy-nodes) 1) first-choice)
                          (progn (rollback)
                                 (setq state 'accept))
                        (setq result (condition-case nil
                                         (lookup-key
                                          map
                                          (vector
                                           (read-event
                                            (substitute-command-keys
                                             (format "%s `%s': `%s' or \\`S-TAB' to cycle; \\`C-g' quits\n%s"
                                                     (combobulate-display-indicator index (length proxy-nodes))
                                                     (combobulate-pretty-print-node current-node nil)
                                                     (mapconcat (lambda (k)
                                                                  (propertize (key-description k) 'face 'help-key-binding))
                                                                (where-is-internal 'next map)
                                                                ", ")
                                                     (if (and flash-node combobulate-flash-node)
                                                         (or (combobulate-draw-node-tree (combobulate-proxy-to-tree-node current-node))
                                                             "")
                                                       "")))
                                            nil nil)))
                                       (quit 'cancel)))
                        (pcase result
                          ('prev
                           (commit)
                           (setq index (mod (1- index) (length proxy-nodes)))
                           (throw 'next nil))
                          ('next
                           (commit)
                           (setq index (mod (1+ index) (length proxy-nodes)))
                           (throw 'next nil))
                          ('done
                           (rollback)
                           (combobulate-message "Committing" current-node)
                           (setq state 'accept))
                          ((pred functionp)
                           (funcall result)
                           (rollback)
                           (throw 'next nil))
                          (_
                           (combobulate-message "Cancelling...")
                           (setq state 'abort)
                           (rollback)
                           (keyboard-quit)))))))))
          (quit nil))
      (error "There are no choices to make."))
    ;; Determine where point is placed on exit.
    (cond
     ((and (eq state 'abort) reset-point-on-abort) (goto-char pt))
     ((and (eq state 'accept) reset-point-on-accept) (goto-char pt)))
    current-node))

(defun combobulate-clone-node-dwim (&optional arg)
  "Clone node at point ARG times."
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:procedures combobulate-navigation-sibling-procedures)
      (combobulate-proffer-choices
       (seq-sort #'combobulate-node-larger-than-node-p (combobulate--get-all-navigable-nodes-at-point))
       (lambda (node mark-highlighted-fn move-fn mark-deleted-fn)
         (funcall mark-deleted-fn)
         (combobulate--clone-node node (combobulate-node-start node))
         (funcall mark-highlighted-fn)
         (funcall move-fn))
       :reset-point-on-abort t))))

(defun combobulate-envelop-node (template node mark-node point-placement)
  "Insert tempo TEMPLATE around NODE.

If MARK-NODE is non-nil, then mark the node, which will then be
available to tempo as the `r' identifier.  If nil, the region is
kept as-is.

POINT-PLACEMENT must be one of `start', `end', or `stay'. `stay'
does not move point to either of NODE's boundaries."
  (interactive)
  ;; reset. if it is non-nil after the template, go to it.
  (setq combobulate--tempo-point-pos nil)
  ;; required as it won't display otherwise.
  (let ((tempo-interactive t)
        (tempo-marks)
        (start (combobulate-node-start node)) (end (combobulate-node-end node))
        (tempo-user-elements '(combobulate--tempo-handle-element-C
                               combobulate--tempo-handle-element-c
                               combobulate--tempo-handle-element-@
                               combobulate--tempo-handle-element-y>)))
    (save-excursion
      ;; If we are asked to mark the node, we do. If not, we still go to
      ;; the beginning
      (if mark-node
          (let ((inhibit-message t))
            (progn (combobulate--mark-node node t)
                   (setq start (point-marker))
                   (setq end (mark-marker))))
        (cond
         ;; nothing to do if point is `stay'.
         ((member point-placement '(start end))
          (combobulate--goto-node node (eq point-placement 'end)))))
      (tempo-insert-template template (or mark-node mark-active)))
    (when combobulate--tempo-point-pos
      (goto-char combobulate--tempo-point-pos))
    (when combobulate-envelope-indent-region-function
      (funcall combobulate-envelope-indent-region-function start end))))

(defun combobulate-get-envelope-by-name (name)
  "Find an envelope with `:name' equal to NAME."
  (seq-find (lambda (envelope) (equal (plist-get envelope :name) name))
            combobulate-manipulation-envelopes))

(defun combobulate-get-envelope-function-by-name (name)
  "Find an envelope with `:name' equal to NAME."
  (when-let (env (combobulate-get-envelope-by-name name))
    (symbol-function (plist-get env :template-symbol))))

(defun combobulate-apply-envelope (envelope &optional node)
  "Envelop NODE near point with ENVELOPE.

Envelopes fail if point is not \"near\" NODE. Set FORCE to
non-nil to override this check."
  (map-let (:nodes :mark-node :description :template-symbol :point-placement :name) envelope
    (unless (and name nodes template-symbol)
      (error "Envelope `%s' is not valid." envelope))
    (with-navigation-nodes (:nodes nodes)
      (if (setq node (or node (combobulate--get-nearest-navigable-node)))
          (progn (combobulate-message "Enveloping" node "in" description)
                 (combobulate-envelop-node
                  template-symbol
                  node
                  mark-node
                  point-placement))
        (error "Cannot apply envelope `%s'. Point must be in one of \
these nodes: `%s'." name nodes)))))

(defun combobulate-execute-envelope (envelope-name &optional node force)
  "Executes any envelope with a `:name' equal to ENVELOPE-NAME.

See `combobulate-apply-envelope' for more information."
  (let ((envelope (combobulate-get-envelope-by-name envelope-name)))
    (if node
        (combobulate-apply-envelope envelope node)
      (combobulate-proffer-choices
       (seq-sort
        (lambda (a b)
          ;; "Smart" sorting that orders by largest node first but
          ;; *only* when the distance from `point' to the start of `a'
          ;; is 0 (i.e., the node starts at point.)
          ;;
          ;; For all other instances, we measure distance from point.
          (if (= (- (combobulate-node-start a) (point)) 0)
              (combobulate-node-larger-than-node-p a b)
            (> (- (combobulate-node-start a) (point))
               (- (combobulate-node-start b) (point)))))
        ;; #'combobulate-node-larger-than-node-p
        (seq-filter (lambda (n)
                      (and (or force (combobulate-point-near-node n))
                           (member (combobulate-node-type n) (plist-get envelope :nodes))))
                    (cons (combobulate-node-at-point)
                          (combobulate-get-parents (combobulate-node-at-point)))))
       (lambda (node mark-highlighted-fn _ mark-deleted-fn)
         ;; mark deleted and highlight first. That way when we apply
         ;; the envelope the overlays expand to match.
         (funcall mark-deleted-fn)
         (funcall mark-highlighted-fn)
         (combobulate-apply-envelope envelope node))
       :reset-point-on-abort t
       :reset-point-on-accept nil))))

(defun combobulate-mark-node-at-point (&optional arg beginning-of-line)
  "Mark the most likely node on or near point ARG times.

The exact node that is marked will depend on the location of
point relative to the nodes in
`combobulate-navigation-default-nodes'."
  (interactive "^p")
  (with-argument-repetition arg
    ;; if the mark's ahead of point then we're at the beginning of the
    ;; region; if so, swap places.
    (when (and mark-active (> (point) (mark)))
      (exchange-point-and-mark))
    (when-let ((parent (car (seq-drop-while
                             #'combobulate-node-in-region-p
                             (combobulate-nav-get-parents (combobulate-node-at-point))))))
      (combobulate--mark-node parent nil beginning-of-line)
      (combobulate--flash-node parent)
      parent)))

(defun combobulate-mark-node-dwim (&optional arg beginning-of-line first-choice)
  "Mark the most likely node on or near point ARG times.

The exact node that is marked will depend on the location of
point relative to the nodes in
`combobulate-navigation-default-nodes'.

If BEGINNING-OF-LINE is t, then the marked node has its point and
mark extended, if possible, to the whole line.

Setting FIRST-CHOICE to t disables proffered choices if there is
more than one."
  (interactive "^p")
  (with-argument-repetition arg
    ;; if the mark's ahead of point then we're at the beginning of the
    ;; region; if so, swap places.
    (when (and mark-active (> (point) (mark)))
      (exchange-point-and-mark))
    (let ((nodes (cons (combobulate--get-nearest-navigable-node)
                       (combobulate-nav-get-parents
                        (combobulate-node-at-point)))))
      ;; maybe mark the thing at point first even though it may (or may
      ;; not) match the extents of a node.
      ;;
      ;; Because we shift point around all the time, we don't want the
      ;; `thing-at-point' machinery to pick up stray "things" at the
      ;; edges of our region; that would be confusing. So only apply
      ;; it if we don't have a region (i.e., the first time we run
      ;; this command)
      (when (and combobulate-mark-node-or-thing-at-point (not (use-region-p)))
        ;; NOTE: this may need refinement over time; for now we
        ;; hardcode the `:type' to be the symbol name of the thing
        ;; we're looking for, though obviously that is most likely a
        ;; made-up node type.
        (when-let ((bounds (bounds-of-thing-at-point combobulate-mark-node-or-thing-at-point))
                   (thing (thing-at-point combobulate-mark-node-or-thing-at-point)))
          (setq nodes (cons (make-combobulate-proxy-node
                             :start (car bounds)
                             :end (cdr bounds)
                             :type (symbol-name combobulate-mark-node-or-thing-at-point)
                             :named nil
                             :node nil
                             :pp thing
                             :text thing)
                            nodes))))
      (combobulate-proffer-choices
       (seq-drop-while #'combobulate-node-in-region-p nodes)
       ;; do not mark `node' for deletion; highlight `node'; or
       ;; move point to `node'.
       (lambda (node _ _ _) (combobulate--mark-node node nil beginning-of-line))
       :reset-point-on-abort t
       :reset-point-on-accept nil
       ;; HACK: This allows repetition of the command that
       ;; `combobulate-mark-node-dwim' is bound to, but this should be
       ;; built into `combobulate-proffer-choices'. Furthermore, is
       ;; `where-is-internal' really the best way to do this?
       :extra-map (mapcar (lambda (key) (cons key 'next))
                          (where-is-internal #'combobulate-mark-node-dwim
                                             combobulate-key-map))
       :flash-node t
       :first-choice first-choice))))

(defun combobulate-mark-defun (&optional arg)
  "Mark defun and place point at the end ARG times.

Uses `combobulate-navigation-defun-nodes' to determine what a
defun is.  Repeat calls expands the scope."
  (interactive "p")
  (with-argument-repetition arg
    (with-navigation-nodes (:nodes combobulate-navigation-defun-nodes :skip-prefix t)
      (unless (combobulate-mark-node-at-point nil t)
        (if (< arg 0)
            (combobulate-navigate-beginning-of-defun)
          (when (and mark-active (> (mark) (point)))
            (exchange-point-and-mark))
          (combobulate-navigate-end-of-defun))))))

(defun combobulate--partition-by-position (self-node query-nodes)
  "Given QUERY-NODES group them relative to SELF-NODE"
  (mapcar (lambda (query-node)
            (let ((node query-node))
              (cond
               ((combobulate-node-before-node-p node self-node)
                (cons 'before node))
               ((combobulate-node-eq node self-node)
                (cons 'self node))
               ((combobulate-node-after-node-p node self-node)
                (cons 'after node))
               ((combobulate-node-contains-node-p self-node node)
                (cons 'around node))
               (t (error "not in partition %s" query-node)))))
          query-nodes))

(defun combobulate-splice-up (&optional arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:nodes
                            (combobulate-procedure-get-activation-nodes
                             combobulate-manipulation-splicing-procedures)
                            :procedures combobulate-manipulation-splicing-procedures)
      (combobulate-splice (combobulate--get-nearest-navigable-node) '(self after around)))))

(defun combobulate-splice-down (&optional arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:nodes
                            (combobulate-procedure-get-activation-nodes
                             combobulate-manipulation-splicing-procedures)
                            :procedures combobulate-manipulation-splicing-procedures)
      (combobulate-splice (combobulate--get-nearest-navigable-node) '(self before around)))))

(defvar combobulate-refactor--copied-values nil)

(defun combobulate-splice (point-node partitions &optional matches)
  "Splice POINT-NODE by PARTITIONS.

PARTITIONS must be an alist of partitions, possibly generated by
`combobulate--partition-by-position'.

Each member of PARTITIONS must be one of:

 `before', to preserve things before the POINT-NODE;
 `after', to preserve things after the POINT-NODE;
 `around' to preserve nodes larger than POINT-NODE;
 `self' to preserve POINT-NODE."
  (let* ((procedure (combobulate-procedure-start point-node))
         (baseline-target nil)
         (tally)
         (matches (or matches (cdr procedure))))
    (combobulate-refactor
        (pcase-dolist (`(,action . ,node) matches)
          (pcase action
            ('@discard
             (mark-node-deleted node)
             (setq baseline-target (combobulate-baseline-indentation node))
             (push (cons action node) tally))
            ('@keep
             (pcase-dolist (`(,position . ,part-node)
                            (combobulate--partition-by-position point-node (list node)))
               (when (member position partitions)
                 (push (cons action node) tally)
                 (pcase-let ((`(,start ,end) (combobulate-extend-region-to-whole-lines
                                              (combobulate-node-start part-node)
                                              (combobulate-node-end part-node))))
                   (mark-copy start end)
                   (mark-node-indent start end
                                     baseline-target
                                     (combobulate-baseline-indentation part-node))))))))
      (combobulate-message (combobulate-tally-nodes tally))
      (commit))
    (combobulate--refactor-insert-copied-values combobulate-refactor--copied-values)))

(defun combobulate-delete-whitespace ()
  "Maybe deletes excess whitespace around point.

Whether this function does anything or not depends on
`combobulate-manipulation-trim-whitespace'."
  (cond ((eq combobulate-manipulation-trim-whitespace 'backward)
         (delete-horizontal-space t))
        ((eq combobulate-manipulation-trim-whitespace 'all)
         (delete-horizontal-space))))


(defun combobulate-delete-empty-lines ()
  "Delete empty lines around point"
  (when combobulate-manipulation-trim-empty-lines
    (delete-blank-lines)))

(defun combobulate--drag (direction)
  "Perform a drag operation on the current navigation node in DIRECTION.

If the current node has no siblings in the specified direction,
an error is raised. If the operation is successful, the cursor is
moved to the modified node."
  (with-navigation-nodes (:nodes combobulate-navigation-drag-parent-nodes)
    (let* ((up (eq direction 'up))
           (node (or (combobulate-node-at-point) (error "No navigable node")))
           (siblings (combobulate-get-immediate-siblings-of-node node)))
      (pcase-let ((`(,prev-sibling ,current-node ,next-sibling) siblings))
        (combobulate-message (format "Dragging %s" (symbol-name direction)) prev-sibling)
        (if current-node
            (combobulate--goto-node current-node)
          (error "Cannot drag from this position"))
        (save-excursion
          (if up
              (progn
                (when (equal current-node prev-sibling)
                  (error "Cannot drag up"))
                (and current-node prev-sibling
                     (combobulate--swap-node-regions current-node prev-sibling)))
            (and current-node next-sibling (combobulate--swap-node-regions current-node next-sibling)))))
      ;; move in the direction of the node
      (setq siblings (combobulate-get-immediate-siblings-of-node (combobulate-node-at-point)))
      (if up (car siblings) (car (last siblings))))))

(defun combobulate-baseline-indentation-default (pos)
  (save-excursion
    (goto-char pos)
    (current-indentation)))

(defun combobulate-baseline-indentation (node-or-pos)
  "Determine the baseline column offset of NODE-OR-POS."
  (funcall combobulate-calculate-indent-function
           (cond ((combobulate-node-p node-or-pos) (combobulate-node-start node-or-pos))
                 ((or (markerp node-or-pos)
                      (integerp node-or-pos))
                  node-or-pos)
                 (t (error "Unknown node-or-pos `%s'" node-or-pos)))))

(defun combobulate-indent-node (node column &optional relative)
  "Indent NODE to COLUMN.

This function rigidly indents NODE by COLUMN while
preserving the relative indentation of each line.

If RELATIVE is non-nil, then COLUMN adds or subtracts from the
baseline indentation (as calculated by
`combobulate-baseline-indentation') instead of absolutely
indenting to that column."
  (let ((baseline-column (combobulate-baseline-indentation node)))
    (combobulate--mark-node node t t)
    (combobulate-indent-region (point) (mark)
                               baseline-column
                               (if relative column
                                 (- column baseline-column)))))

(defun combobulate-indent-region (start end column &optional baseline-column relative)
  "Indent the region between START and END to COLUMN.

If RELATIVE is non-nil, then add or subtract from the current
indentation rather than setting the absolute indentation from the
beginning of the line."
  (let ((baseline-column
         (or baseline-column
             (combobulate-baseline-indentation start))))
    (indent-rigidly start end (if relative column (- column baseline-column)))))

(defun combobulate-take-query-nodes-between (nodes start-label stop-label)
  "Keeps all NODES starting from START-LABEL and ending at STOP-LABEL."
  (let ((filtered) (collect))
    (nreverse (progn
                (catch 'done
                  (pcase-dolist (`(,label . ,node) nodes)
                    (when (and (eq stop-label label) collect)
                      (throw 'done filtered))
                    (when (eq start-label label)
                      (setq collect t))
                    (when collect
                      (push (cons label node) filtered))))
                filtered))))


(defun combobulate-move-node (node to-node placement-query)
  "Move NODE on or near TO-NODE with PLACEMENT-QUERY.

PLACEMENT-QUERY must be either nil, in which case the placement
of NODE is approximated, or a query. The query must contain the
label `@to-node' as a label for the base node from which the
actual location of NODE is determined.

The actual placement itself can be specified -- due to
limitations in the query language -- as either `@before' or
`@after' to indicate whether NODE should be placed before or
after TO-NODE."
  (pcase-let ((`(,position . ,target)
               (or (if placement-query
                       (cdr (combobulate-take-query-nodes-between
                             (combobulate--query-from-node placement-query to-node)
                             'to-node 'to-node))
                     (cons 'after to-node))
                   (error "Cannot find any placement nodes from `%s'" to-node))))
    (unless target
      (error "Cannot move `%s' as there is no valid target." node))
    (let ((target-marker (combobulate--refactor-mark-position
                          (combobulate-node-start target))))
      (combobulate-node-region-lines node)
      (combobulate-indent-node node (combobulate-baseline-indentation target) t)
      (let ((delete-marker (combobulate--refactor-mark-deleted (point) (mark)))
            (s (buffer-substring-no-properties (point) (mark))))
        (deactivate-mark)
        (combobulate--refactor-commit target-marker t)
        (when (eq position 'after)
          (forward-line 1))
        (split-line)
        (save-excursion
          (forward-line 0)
          (insert s))
        (back-to-indentation)
        (combobulate--refactor-commit delete-marker t)))))

(defun combobulate--refactor-commit (ov &optional destroy-overlay)
  (if-let ((actions (overlay-get ov 'combobulate-refactor-actions)))
      (let* ((buf (overlay-buffer ov))
             (ov-start (overlay-start ov))
             (ov-end (overlay-end ov)))
        (when buf
          (dolist (action actions)
            (when combobulate-debug
              (princ (format "action: %s (ov %s)\n" action ov)))
            (pcase action
              (`(copy-region ,target-var)
               (push (buffer-substring-no-properties
                      (max (point-min) ov-start)
                      (min (point-max) (save-excursion
                                         (goto-char ov-end)
                                         (cond
                                          ((looking-at "\n") (1+ ov-end))
                                          ((looking-at " ") (+
                                                             (- (match-end 0)
                                                                (match-beginning 0))
                                                             ov-end))
                                          (t ov-end)))))
                     (symbol-value target-var)))
              ('(delete-region)
               ;; detect single-char overlays with newlines.
               (unless (string-blank-p (buffer-substring ov-start ov-end))
                 (delete-region ov-start ov-end)))
              (`(indent ,pt ,baseline-column)
               (combobulate-indent-region ov-start ov-end pt baseline-column))
              ('(set-point)
               (goto-char ov-start))
              ;; do nothing; it's just a highlight
              ('(highlighted))
              (_ (error "Unknown refactor commit action `%s'" action)))))
        (when destroy-overlay
          (delete-overlay ov)))
    (error "Overlay `%s' does not have a `combobulate-refactor-action's property" ov)))

(defun combobulate--refactor-mark-position (pt)
  (let ((ov (make-overlay pt pt)))
    (overlay-put ov 'combobulate-refactor-actions '((set-point)))
    ov))

(defun combobulate--refactor-mark-deleted (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'combobulate-refactor-actions '((delete-region)))
    (when combobulate-debug
      (overlay-put ov 'face 'smerge-refined-removed))
    ov))

(defun combobulate--refactor-mark-highlighted (beg end &optional advance)
  (let ((ov (make-overlay beg end nil advance advance)))
    (overlay-put ov 'combobulate-refactor-actions '((highlighted)))
    (overlay-put ov 'face 'combobulate-refactor-highlight-face)
    ov))

(defun combobulate--refactor-mark-copy (beg end &optional target-var)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'combobulate-refactor-actions `((copy-region ,target-var)))
    (when combobulate-debug
      (overlay-put ov 'face 'diff-added))
    ov))

(defun combobulate--refactor-mark-indent (beg end pt baseline-column)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'combobulate-refactor-actions `((indent ,pt ,baseline-column)))
    (when combobulate-debug
      (overlay-put ov 'face 'diff-changed))
    ov))

(defun combobulate--refactor-mark-generic (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'combobulate-refactor-actions '((generic)))
    (when combobulate-debug
      (overlay-put ov 'face 'diff-changed))
    ov))

(defun combobulate-reinitialize-parser ()
  "Force tree-sitter to reparse the buffer."
  (interactive)
  ;; Required after editing the buffer programmatically as the
  ;; tree-sitter integration with Emacs is buggy.
  (dolist (parser (combobulate-parser-list))
    (combobulate-parser-create (combobulate-parser-language parser) nil t)
    (combobulate-parser-delete parser))
  (font-lock-flush))

(defun combobulate-drag-up (&optional arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:nodes combobulate-navigation-drag-parent-nodes)
      (combobulate-visual-move-to-node (combobulate--drag 'up)))))

(defun combobulate-drag-down (&optional arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:nodes combobulate-navigation-drag-parent-nodes)
      (combobulate-visual-move-to-node (combobulate--drag 'down)))))

(provide 'combobulate-manipulation)
;;; combobulate-manipulation.el ends here


