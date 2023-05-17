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
(declare-function combobulate-envelope-expand-instructions "combobulate-envelope")
(declare-function combobulate-display-draw-node-tree "combobulate-display")

(defvar combobulate-key-map)

(defun combobulate--refactor-insert-copied-values (values)
  (let ((start-marker (point-marker)) (after-marker) (pt))
    ;; move this into a distinct function that can reproduce stored
    ;; input.
    (goto-char (point-marker))
    (skip-syntax-backward " ")
    (just-one-space 0)
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
  (declare (indent nil) (debug (form body)))
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
                  (mark-range-move (beg end position)
                    (add-marker (combobulate--refactor-mark-move beg end position)))
                  (mark-range-deleted (beg end)
                    (add-marker (combobulate--refactor-mark-deleted beg end)))
                  (mark-range-highlighted (beg end &optional face advance)
                    (add-marker (combobulate--refactor-mark-highlighted beg end face advance)))
                  (mark-copy (start end)
                    (add-marker (combobulate--refactor-mark-copy start end 'combobulate-refactor--copied-values)))
                  (mark-node-copy (n)
                    (mark-copy (combobulate-node-start n) (combobulate-node-end n)))
                  (mark-node-indent (beg end target-pt baseline-column)
                    (add-marker (combobulate--refactor-mark-indent beg end target-pt baseline-column)))
                  (mark-node-deleted (n)
                    (add-marker (combobulate--refactor-mark-deleted (combobulate-node-start n)
                                                                    (combobulate-node-end n))))
                  (mark-node-highlighted (n &optional face advance)
                    (mark-range-highlighted (combobulate-node-start n)
                                            (combobulate-node-end n)
                                            face
                                            advance))
                  (mark-cursor (pt)
                    (add-marker (combobulate--refactor-mark-cursor pt)))
                  (mark-node-cursor (n)
                    (mark-cursor (combobulate-node-start n)))
                  (mark-field (pt tag &optional text transformer-fn)
                    (add-marker (combobulate--refactor-mark-field
                                 pt tag (or text (symbol-name tag))
                                 transformer-fn)))
                  (update-field (tag text)
                    (seq-filter
                     (lambda (ov) (let ((actions (overlay-get ov 'combobulate-refactor-action)))
                               (combobulate--refactor-update-field ov tag text)))
                     ,--markers))
                  (mark-point (&optional pt)
                    (add-marker (combobulate--refactor-mark-position (or pt (point)))))
                  (rollback ()
                    (mapc (lambda (ov) (delete-overlay ov))
                          ,--markers)
                    (setq ,--markers nil))
                  (commit ()
                    (setq combobulate-refactor--copied-values nil)
                    (mapc (lambda (ov) (combobulate--refactor-commit ov t))
                          (seq-sort (lambda (a b)
                                      (and a b
                                           (overlayp a)
                                           (overlayp b)
                                           (> (overlay-start a)
                                              (overlay-end b))))
                                    ,--markers))
                    ;; clean up.
                    (mapc (lambda (ov) (delete-overlay ov))
                          ,--markers)
                    (setq ,--markers nil)))
         (condition-case nil
             (prog1 (atomic-change-group
                      ,@body)
               (when ,--markers
                 (error "Uncommitted changes: %s"
                        (prog1 ,--markers
                          (combobulate--refactor-clear-overlays
                           (combobulate--refactor-get-all-overlays))))))
           (quit (rollback)))))))

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
  (if nodes
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
                     "; "))
    "zero"))

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


(defun combobulate-edit-node-type-dwim (arg)
  "Edit nodes of the same type by node locus.

This looks for nodes of any type found in `combobulate-navigation-default-nodes'."
  (interactive "P")
  (with-navigation-nodes (:nodes combobulate-navigation-default-nodes)
    (if-let ((node (combobulate--get-nearest-navigable-node)))
        (combobulate-edit-identical-nodes
         node arg (lambda (tree-node) (and(equal (combobulate-node-type node)
                                            (combobulate-node-type tree-node))
                                     (equal (combobulate-node-field-name node)
                                            (combobulate-node-field-name tree-node)))))
      (error "Cannot find any editable nodes here"))))

(defun combobulate-edit-node-by-text-dwim (arg)
  "Edit nodes with the same text by node locus.

This looks for nodes of of any type found in
`combobulate-navigation-default-nodes' that have the same text as
the node at point."
  (interactive "P")
  (if-let ((node (combobulate-node-at-point nil t)))
      (combobulate-edit-identical-nodes
       node arg (lambda (tree-node) (equal (combobulate-node-text tree-node)
                                      (combobulate-node-text node))))
    (error "Cannot find any editable nodes here")))

(defun combobulate-edit-identical-nodes (node &optional point-at-end match-fn)
  "Edit nodes identical to NODE if they match MATCH-FN.

If POINT-AT-END is non-nil, then point is placed at the end of
the node boundary of each match instead of the beginning.

The locus of editable nodes is determined by NODE's parents and
is selectable.

MATCH-FN takes one argument, a node, and returns non-nil if it is
a match."
  (let ((matches)
        ;; default to 1 "match" as there's no point in creating
        ;; multiple cursors when there's just one match
        (ct 1)
        (group-node)
        (grouped-matches))
    (dolist (start-node (combobulate-get-parents node))
      (setq matches (seq-uniq (flatten-tree (combobulate-induce-sparse-tree start-node match-fn))
                              ;; Remove nodes that share the same node
                              ;; range: they are most probably the
                              ;; same node. If we do not do this, we
                              ;; would end up with several multiple
                              ;; cursors at the exact same position.
                              (lambda (node-a node-b) (equal (combobulate-node-range node-a)
                                                        (combobulate-node-range node-b)))))
      ;; this catches parent nodes that do not add more, new, nodes to
      ;; the editing locus by filtering them out.
      (when (> (length matches) ct)
        (setq ct (length matches))
        (push (cons start-node matches) grouped-matches)))
    (combobulate-refactor
     (let ((matches
            (cdr
             (assoc
              (combobulate-proxy-to-tree-node
               (combobulate-proffer-choices
                (reverse (mapcar 'car grouped-matches))
                (lambda (node mark-highlighted-fn move-fn mark-deleted-fn)
                  (princ (format "Editing %s in %s%s\n"
                                 (combobulate-pretty-print-node-type node)
                                 (combobulate-proxy-to-tree-node node)
                                 (and (combobulate-node-field-name node)
                                      (format " (%s)"
                                              (combobulate-node-field-name node)))))
                  ;; rollback the outer
                  ;; `combobulate-refactor' call so
                  ;; the node cursors we place below
                  ;; are properly erased.
                  (rollback)
                  ;; place a fake cursor at every
                  ;; node to indicate where the
                  ;; matching nodes are.
                  (mapc #'mark-node-cursor
                        (cdr (assoc (combobulate-proxy-to-tree-node node)
                                    grouped-matches)))
                  ;; indicate the locus of editing
                  ;; by highlighting the entire node
                  ;; boundary.
                  (setq group-node node)
                  (funcall mark-highlighted-fn))
                :unique-only nil
                :prompt-description
                (format "Edit %s in"
                        (propertize (combobulate-pretty-print-node-type node)
                                    'face 'combobulate-tree-branch-face))))
              grouped-matches))))
       (rollback)
       (when matches
         (combobulate--mc-edit-nodes matches point-at-end)
         (combobulate-message
          (format "Editing %s in `%s'"
                  (combobulate-tally-nodes matches)
                  (propertize
                   (combobulate-node-type group-node)
                   'face 'combobulate-active-indicator-face))))))))

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
     (cons (combobulate-indent-string
            (combobulate-indent-string--strip-whitespace (car lines))
            target-col)
           (cdr lines))
     "\n")))

(defun combobulate-indent-string--strip-whitespace (s &optional count)
  "Strip S of whitespace, possibly up to COUNT whitespace characters."
  (string-trim-left
   s (if count (rx-to-string `(** 0 ,(abs count) space))
       (rx bol (* space)))))

(defun combobulate-indent-string--count-whitespace (s)
  (- (length s) (length (combobulate-indent-string--strip-whitespace s))))

(defun combobulate-indent-string (text target-col &optional absolute)
  "Ensure the indent of TEXT is set to TARGET-COL.

If ABSOLUTE is t, then every line is adjusted like this:

The first line of text is used as the zero point and indented to
TARGET-COL, and every subsequent line's indentation is indented
relative to that one such that their relative indentation to the
first line is preserved."
  (let* ((lines (split-string text "\n"))
         (baseline-col (combobulate-indent-string--count-whitespace (car lines))))
    ;; calculate the indentation at the beginning of the line, if
    ;; any.
    (string-join
     (mapcar (lambda (line)
               (if absolute
                   (combobulate-indent-string
                    (combobulate-indent-string--strip-whitespace line baseline-col)
                    target-col nil)
                 (if (> target-col 0)
                     (concat (make-string target-col ? ) line)
                   (combobulate-indent-string--strip-whitespace line target-col))))
             lines)
     "\n")))

(defun combobulate--clone-node (node position)
  "Clone NODE and place it at POSITION."
  (combobulate--place-node-or-text position node))

(defun combobulate--place-node-or-text (position node-or-text &optional mode no-trailing-newline)
  "Place NODE-OR-TEXT at POSITION.

NODE-OR-TEXT must be a valid node or a string.

If NODE-OR-TEXT is a NODE, then its first-line indentation is
calculated first and the text indented properly relative to
POSITION.

If NODE-OR-TEXT is a string, then all indentation must be handled
by the caller. The string is inserted at POSITION.

If MODE is non-nil, then it must be either `newline' or
`inline'. Where `newline' forces the placement of NODE-OR-TEXT on
a new line. If MODE is `inline' then it is places inline
alongside other nodes around POSITION.

If NO-TRAILING-NEWLINE is t, then no trailing newline is inserted
after NODE-OR-TEXT."
  (let* ((col (save-excursion (goto-char position)
                              (current-column)))
         (node-text))
    (cond
     ((stringp node-or-text)
      (setq node-text (combobulate-indent-string node-or-text col t)))
     ((or (combobulate-node-p node-or-text)
          (combobulate-proxy-node-p node-or-text))
      (let ((sequence-separator
             ;; attempt to guess the sequence separator, almost always
             ;; an anonymous node, that (usually!) follows nodes --
             ;; think array/list separators; semicolons after
             ;; expressions, etc. -- and ensure it is appended to the
             ;; cloned text. This is of course not the proper way to
             ;; do this: the `xxx-grammar.json' files contain the
             ;; rules for sequences and the character to use. This is
             ;; merely the 90% solution that works for most, but not
             ;; all, instances.
             (save-excursion
               (combobulate--goto-node node-or-text t)
               (let ((node-after (combobulate-node-on (point) (1+ (point)) nil nil)))
                 ;; we only care about unnamed nodes: yes, it's
                 ;; possible named nodes are used for sequence
                 ;; separators, I suppose, but this is not handled
                 ;; here at all.
                 (if (combobulate-node-named-p node-after)
                     ""
                   (combobulate-node-text node-after)))))
            (node-col (save-excursion
                        (combobulate--goto-node node-or-text)
                        (current-indentation))))
        (setq node-text (combobulate-indent-string
                         ;; the first line may not include all of its
                         ;; indentation because the node extents won't
                         ;; include it. This fixes it so it does.
                         (combobulate-indent-string-first-line
                          (concat (combobulate-node-text node-or-text) sequence-separator)
                          node-col)
                         (- col node-col)))))
     (t (error "Cannot place node or text `%s'" node-or-text)))
    (goto-char position)
    ;; If a node has nothing but whitespace preceding it, then it's a
    ;; "newline-delimited" node. Newline-delimited nodes are regular
    ;; nodes that exist, usually, on a line of their own. That is on
    ;; contrast to inline-delimited nodes that are placed to the left
    ;; or right on a line with other nodes.
    (if (and (or (eq mode 'newline) (null mode))
             (combobulate-before-point-blank-p position))
        ;; newline-delimited node
        (progn
          (unless no-trailing-newline (split-line 0))
          (combobulate--refactor-insert-copied-values
           (list node-text)))
      ;; inline-delimited node
      (save-excursion
        (insert node-text)
        (just-one-space 0)
        (unless (looking-at "\\s-")
          (insert " ")))
      (just-one-space))))

(defun combobulate--mark-node (node &optional swap beginning-of-line)
  "Mark NODE in the current buffer.

See `combobulate--mark-extent' for argument explanations."
  (when node
    (combobulate--mark-extent (combobulate-node-start node)
                              (combobulate-node-end node)
                              swap
                              beginning-of-line)))

(defun combobulate--delete-text (beg end &optional correct-indentation delete-blank-lines)
  (save-excursion
    (let ((text (save-excursion
                  (prog1 (delete-and-extract-region beg end)))))
      (prog1 (if correct-indentation
                 (combobulate-indent-string-first-line
                  text
                  (save-excursion
                    (goto-char beg)
                    (current-indentation)))
               text)
        (save-excursion
          ;; Only trim lines if there is more than one as
          ;; `delete-blank-lines', in case there is only one, will
          ;; delete that blank line. That can have very bad
          ;; repercussions if the deletion is used as part of a
          ;; greater chain of operations, like moving text in a
          ;; refactor
          (if (= (combobulate-string-count "\n" text) 0)
              (save-restriction
                (goto-char end)
                (narrow-to-region (line-beginning-position) (point-max))
                (when (and delete-blank-lines (or (> (combobulate-count-lines-ahead (point)) 1)
                                                  (= (combobulate-string-count "\n" text) 0)))
                  (delete-blank-lines)))
            (save-restriction
              (narrow-to-region (line-beginning-position) (point-max))
              (when (and delete-blank-lines (or (> (combobulate-count-lines-ahead (point)) 1)
                                                (= (combobulate-string-count "\n" text) 0)))
                (delete-blank-lines)))))))))

(defun combobulate--delete-node (node &optional correct-indentation delete-blank-lines)
  "Deletes NODE in the current buffer and returns its text.

If CORRECT-INDENTATION is t, then the node's first-line
indentation is set according to its current indentation in the
buffer.

If DELETE-BLANK-LINES is t, then all blank lines left behind by
the deleted node are removed."
  (when node
    (combobulate--delete-text (combobulate-node-start node)
                              (combobulate-node-end node)
                              correct-indentation
                              delete-blank-lines)))

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

(defvar combobulate-envelope-static)

(defvar combobulate-proffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-g" 'cancel)
    (define-key map "\M-c" 'recursive-edit)
    (define-key map "\C-l" #'recenter)
    (define-key map [return] 'done)
    (define-key map [tab] 'next)
    (define-key map [S-iso-lefttab] 'prev)
    (define-key map (kbd "S-<tab>") 'prev)
    map)
  "Keymap for `combobulate-proffer-choices'.")

(cl-defun combobulate-proffer-choices (nodes action-fn &key (first-choice nil)
                                             (reset-point-on-abort t) (reset-point-on-accept nil)
                                             (prompt-description nil)
                                             (extra-map nil) (flash-node nil) (unique-only t))
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

`:extra-map' is a list of cons cells consisting of (KEY . COMMAND).

`:prompt-description' is a string that is displayed in the prompt."
  (let ((proxy-nodes
         (and nodes (combobulate-make-proxy
                     ;; strip out duplicate nodes. That
                     ;; includes nodes that are duplicates
                     ;; of one another; however, we also
                     ;; strip out nodes that share the
                     ;; same range extent.
                     (if unique-only
                         (seq-uniq nodes (lambda (a b)
                                           (or (equal a b)
                                               (equal (combobulate-node-range a)
                                                      (combobulate-node-range b)))))
                       nodes))))
        (result) (state 'continue) (current-node)
        (index 0) (pt (point)) (raw-event)
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
                     ;; if we have just one item, or if
                     ;; `:first-choice' is non-nil, we pick the first
                     ;; item in `proxy-nodes'
                     (if (or (= (length proxy-nodes) 1) first-choice)
                         (progn (rollback)
                                (setq state 'accept))
                       (setq result
                             (condition-case nil
                                 ;; `lookup-key' is funny and it only
                                 ;; takes a vector of an event.
                                 (lookup-key
                                  map
                                  (vector
                                   ;; we need to preserve the raw
                                   ;; event. If we want the event
                                   ;; read by `read-event' to pass
                                   ;; through to the event loop
                                   ;; unscathed then we need the raw
                                   ;; version.
                                   (setq raw-event
                                         (read-event
                                          (substitute-command-keys
                                           (format "%s %s`%s': `%s' or \\`S-TAB' to cycle; \\`C-g' quits; rest accepts.%s"
                                                   (combobulate-display-indicator index (length proxy-nodes))
                                                   (concat (or prompt-description "") " ")
                                                   (propertize (combobulate-pretty-print-node current-node) 'face
                                                               'combobulate-tree-highlighted-node-face)
                                                   (mapconcat (lambda (k)
                                                                (propertize (key-description k) 'face 'help-key-binding))
                                                              ;; messy; is this really the best way?
                                                              (where-is-internal 'next map)
                                                              ", ")
                                                   (if (and flash-node combobulate-flash-node)
                                                       (concat "\n"
                                                               (or (combobulate-display-draw-node-tree
                                                                    (combobulate-proxy-to-tree-node current-node))
                                                                   ""))
                                                     "")))
                                          nil nil))))
                               ;; if `condition-case' traps a quit
                               ;; error, then map it into the symbol
                               ;; `cancel', which corresponds to the
                               ;; equivalent event in the state
                               ;; machine below.
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
                         ('cancel
                          (combobulate-message "Cancelling...")
                          (setq state 'abort)
                          (rollback)
                          (keyboard-quit))
                         (_
                          (combobulate-message "Committing...")
                          ;; pushing `raw-event' to
                          ;; `unread-command-events' allows for a
                          ;; seamless exit out of the proffer prompt
                          ;; by preserving the character the user
                          ;; typed to 'break out' of `read-event'
                          ;; earlier.
                          (push raw-event unread-command-events)
                          (rollback)
                          (setq state 'accept)))))))))
          (quit nil))
      (error "There are no choices to make"))
    ;; Determine where point is placed on exit and whether we return
    ;; the current node or not.
    (cond
     ((and (eq state 'abort))
      (when reset-point-on-abort (goto-char pt))
      nil)
     ((and (eq state 'accept))
      (when reset-point-on-accept
        (goto-char pt))
      current-node)
     (t (error "Unknown termination state `%s'" state)))))

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
         (funcall move-fn)
         (combobulate-skip-whitespace-forward))
       :reset-point-on-abort t))))

(defun combobulate-envelop-node (template node mark-node point-placement)
  "Insert Combobulate TEMPLATE around NODE.

If MARK-NODE is non-nil, then mark the node, which will then be
available to tempo as the `r' identifier.  If nil, the region is
kept as-is.

POINT-PLACEMENT must be one of `start', `end', or `stay'. `stay'
does not move point to either of NODE's boundaries."
  (interactive)
  (save-excursion
    ;; If we are asked to mark the node, we do. If not, we still go to
    ;; the beginning
    (if mark-node
        (combobulate--mark-node node t)
      (cond
       ;; nothing to do if point is `stay'.
       ((member point-placement '(start end))
        (combobulate--goto-node node (eq point-placement 'end)))))
    (combobulate-envelope-expand-instructions template)))

(defun combobulate-get-envelope-by-name (name)
  "Find an envelope with `:name' equal to NAME."
  (seq-find (lambda (envelope) (equal (plist-get envelope :name) name))
            (append combobulate-manipulation-envelopes
                    (combobulate-get-envelopes-by-major-mode))))

(defun combobulate-get-envelopes-by-major-mode ()
  (mapcan
   (lambda (parser) (alist-get (combobulate-parser-language parser)
                          combobulate-manipulation-envelopes-custom))
   (combobulate-parser-list)))

(defun combobulate-get-envelope-function-by-name (name)
  "Find an envelope with `:name' equal to NAME."
  (when-let (env (combobulate-get-envelope-by-name name))
    (symbol-function (plist-get env :template-symbol))))

(defun combobulate-apply-envelope (envelope &optional node)
  "Envelop NODE near point with ENVELOPE.

Envelopes fail if point is not \"near\" NODE. Set FORCE to
non-nil to override this check."
  (map-let (:nodes :mark-node :description :template :point-placement :name) envelope
    (unless (and name)
      (error "Envelope `%s' is not valid." envelope))
    (with-navigation-nodes (:nodes nodes)
      (if (setq node (or node (combobulate--get-nearest-navigable-node)))
          (progn (combobulate-message "Enveloping" node "in" description)
                 (combobulate-envelop-node
                  template
                  node
                  mark-node
                  point-placement))
        (error "Cannot apply envelope `%s'. Point must be in one of \
these nodes: `%s'." name nodes)))))

(defun combobulate-execute-envelope (envelope-name &optional node force)
  "Executes any envelope with a `:name' equal to ENVELOPE-NAME.

See `combobulate-apply-envelope' for more information."
  (let ((envelope (combobulate-get-envelope-by-name envelope-name))
        (chosen-node) (accepted nil))
    (unless envelope
      (error "There is no such envelope registered with the name `%s'"
             envelope-name))
    (if node
        (goto-char (cdr (combobulate-apply-envelope envelope node)))
      (let ((combobulate-envelope-static t)
            (envelope-nodes (plist-get envelope :nodes))
            (change-group (prepare-change-group))
            (undo-outer-limit nil)
            (undo-limit most-positive-fixnum)
            (undo-strong-limit most-positive-fixnum))
        (unwind-protect
            (progn
              ;; use a change group to ensure we revert the proffered
              ;; (and selected) choice immediately after. this is a
              ;; hacky way of displaying an expansion (in conjunction
              ;; with `combobulate-envelope-static' set to t) and not
              ;; activate interactive prompts.
              (activate-change-group change-group)
              (setq chosen-node
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
                      ;; if we don't have any assigned envelope nodes,
                      ;; create a proxy node at point; that node (and
                      ;; thus `point') will instead be where the
                      ;; envelope is inserted.
                      (if envelope-nodes
                          (seq-filter (lambda (n)
                                        (and (or force (combobulate-point-near-node n))
                                             (member (combobulate-node-type n)
                                                     envelope-nodes)))
                                      (cons (combobulate-node-at-point)
                                            (combobulate-get-parents (combobulate-node-at-point))))
                        (list (combobulate-make-proxy-point-node))))
                     (lambda (node mark-highlighted-fn _ mark-deleted-fn)
                       ;; mark deleted and highlight first. That way when we apply
                       ;; the envelope the overlays expand to match.
                       (funcall mark-deleted-fn)
                       (let ((ov (funcall mark-highlighted-fn)))
                         (seq-let [[start &rest end] &rest pt]
                             (combobulate-apply-envelope envelope node)
                           (goto-char pt)
                           ;; lil' hack. The extent of the node is not
                           ;; the same as the envelope we just
                           ;; applied.
                           (move-overlay ov start end))))
                     :reset-point-on-abort t
                     :reset-point-on-accept nil))
              (setq accepted t)
              (cancel-change-group change-group))
          (cancel-change-group change-group)))
      ;; here we simply repeat what ever the selected choice was, as
      ;; an explicit node skips the proffering process entirely.
      (when (and chosen-node accepted)
        (combobulate-execute-envelope envelope-name chosen-node)))))

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
                             :field nil
                             :pp thing
                             :text thing)
                            nodes))))
      (combobulate-proffer-choices
       (seq-drop-while #'combobulate-node-in-region-p nodes)
       ;; do not mark `node' for deletion; highlight `node'; or
       ;; move point to `node'.
       (lambda (node &rest _) (combobulate--mark-node node nil beginning-of-line))
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

(defun combobulate-move-past-close-and-reindent (&optional arg)
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-move-to-node (combobulate--navigate-up))
    (if-let (sibling (combobulate--navigate-next))
        (progn
          (combobulate-move-to-node sibling)
          (split-line))
      (let ((col (current-column)))
        (combobulate-move-to-node (combobulate--navigate-self-end) t)
        (newline)
        (indent-to col)))))

(defun combobulate--yeet (point-node)
  (let* ((source-node (or (car (reverse (combobulate--get-siblings point-node)))
                          (error "No valid sibling node.")))
         (target-node
          (save-excursion
            (combobulate-move-to-node
             (with-navigation-nodes (:nodes combobulate-navigation-parent-child-nodes)
               (combobulate-nav-get-parent point-node)))
            (or (combobulate-make-proxy (combobulate--get-sibling
                                         (combobulate-node-at-point)
                                         'forward))
                (error "No valid sibling node.")))))
    (save-excursion
      (let ((pos))
        (combobulate-refactor
         (mark-range-move
          (combobulate-node-start source-node)
          (combobulate-node-end source-node)
          (progn
            (combobulate-move-to-node target-node)
            (back-to-indentation)
            (split-line 1)
            (setq pos (point-marker))))
         (commit))
        (delete-blank-lines)
        (goto-char pos)
        (delete-blank-lines)))))

(defun combobulate--yoink (point-node)
  (let* ((point-sibling (car (reverse (combobulate--get-siblings point-node))))
         (target-node
          (save-excursion
            (combobulate-move-to-node
             (with-navigation-nodes (:nodes combobulate-navigation-parent-child-nodes)
               (combobulate-nav-get-parent point-node)))
            (or (combobulate-make-proxy (combobulate--get-sibling
                                         (combobulate-node-at-point)
                                         'forward))
                (error "No valid sibling node.")))))
    (save-excursion
      (let ((pos-col))
        (combobulate-refactor
         (mark-range-move
          (combobulate-node-start target-node)
          (combobulate-node-end target-node)
          (progn
            (setq pos-col (current-indentation))
            ;; find the right place to place the newline and indent.
            (if point-sibling
                (combobulate-move-to-node point-sibling t)
              (end-of-line))
            (when (combobulate-after-point-blank-p (point))
              (newline)
              (indent-to pos-col))
            (point)))
         (commit))))))

(defun combobulate-yoink-forward (arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:procedures combobulate-navigation-sibling-procedures)
      (combobulate--yoink (combobulate--get-nearest-navigable-node)))))

(defun combobulate-yeet-forward (arg)
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:procedures combobulate-navigation-sibling-procedures)
      (combobulate--yeet (combobulate--get-nearest-navigable-node)))))

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
              (`(move ,beg ,end ,position)
               (let ((pos position)
                     (trailing-newlines (combobulate-count-lines-ahead end)))
                 (save-excursion
                   (goto-char position)
                   (combobulate--place-node-or-text
                    position
                    (prog1 (combobulate--delete-text beg end t t)
                      (goto-char pos))
                    nil
                    (> trailing-newlines 1)))))
              ('(delete-region)
               ;; detect single-char overlays with newlines.
               (unless (string-blank-p (buffer-substring ov-start ov-end))
                 (delete-region ov-start ov-end)))
              (`(indent ,pt ,baseline-column)
               (combobulate-indent-region ov-start ov-end pt baseline-column))
              ('(set-point)
               (goto-char ov-start))
              (`(field . ,_))
              ;; do nothing; it's just a highlight
              ('(highlighted))
              ;; cursors are just for display.
              ('(cursor))
              (_ (error "Unknown refactor commit action `%s'" action)))))
        (when destroy-overlay
          (delete-overlay ov)))
    (error "Overlay `%s' does not have a `combobulate-refactor-action's property" ov)))

(defun combobulate--refactor-mark-position (pt)
  (let ((ov (make-overlay pt pt)))
    (overlay-put ov 'combobulate-refactor-actions '((set-point)))
    ov))

(defun combobulate--refactor-mark-move (beg end target-position)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'combobulate-refactor-actions `((move ,beg ,end ,target-position)))
    (when combobulate-debug
      (overlay-put ov 'face 'smerge-refined-changed))
    ov))

(defun combobulate--refactor-mark-deleted (beg end)
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'combobulate-refactor-actions '((delete-region)))
    (when combobulate-debug
      (overlay-put ov 'face 'smerge-refined-removed))
    ov))

(defun combobulate--refactor-mark-highlighted (beg end &optional face advance)
  (let ((ov (make-overlay beg end nil advance advance)))
    (overlay-put ov 'combobulate-refactor-actions '((highlighted)))
    (overlay-put ov 'face (or face 'combobulate-refactor-highlight-face))
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

(defun combobulate--refactor-mark-field (pt tag text &optional transformer-fn)
  (let ((ov (make-overlay pt pt nil t nil)))
    ;; args 2 and 3 refer to respectively `tag' and the default value.
    (overlay-put ov 'combobulate-refactor-actions `((field ,tag ,text)))
    (overlay-put ov 'face 'combobulate-refactor-field-face)
    (overlay-put ov 'combobulate-refactor-field-transformer-fn transformer-fn)
    (combobulate--refactor-set-field ov text (symbol-name tag))
    ov))

(defun combobulate--refactor-mark-cursor (pt)
  (let ((ov (make-overlay pt (1+ pt) nil t nil)))
    ;; args 2 and 3 refer to respectively `tag' and the default value.
    (overlay-put ov 'combobulate-refactor-actions `((cursor)))
    (overlay-put ov 'face 'combobulate-refactor-cursor-face)
    ov))

(defun combobulate--refactor-set-field (ov text &optional default-text)
  (let ((start (overlay-start ov))
        (s (if (length= text 0) default-text text))
        (transformer-fn (or (overlay-get ov 'combobulate-refactor-field-transformer-fn)
                            #'identity)))
    (delete-region start (overlay-end ov))
    (goto-char start)
    (insert (funcall transformer-fn s))
    (move-overlay ov start (point))))

(defun combobulate--refactor-field-has-tag-p (ov tag &optional action-name)
  (let ((actions (overlay-get ov 'combobulate-refactor-actions))
        (match))
    (pcase-dolist (`(,action ,ov-tag _) actions)
      (when (and (eq action (or action-name 'field)) (eq ov-tag tag))
        (setq match t)))
    match))

(defun combobulate--refactor-update-field (ov tag text &optional default-text)
  (let ((actions (overlay-get ov 'combobulate-refactor-actions)))
    (when (combobulate--refactor-field-has-tag-p ov tag)
      (map-put! actions 'field (cons tag text))
      (overlay-put ov 'combobulate-refactor-actions actions)
      (combobulate--refactor-set-field ov text default-text))
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


