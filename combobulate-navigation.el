;;; combobulate-navigation.el --- navigational aids for combobulate  -*- lexical-binding: t; -*-

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
(require 'generator)
(require 'treesit)
(require 'combobulate-settings)
(require 'combobulate-misc)
(require 'combobulate-interface)
(require 'map)


(declare-function combobulate-procedure-start-aggressive "combobulate-manipulation")
(declare-function combobulate-procedure-get-activation-nodes "combobulate-manipulation")

(defsubst combobulate-group-nodes (labelled-nodes &optional group-fn)
  "Group LABELLED-NODES from a query by their label.

If GROUP-FN is nil the default is to use the `car' of each cons
in LABELLED-NODES and group by the label.

If GROUP-FN is non-nil it must be a function that takes a single
cons cell as an argument."
  (seq-sort
   (lambda (a b) (> (length a) (length b)))
   (seq-group-by (or group-fn #'car) labelled-nodes)))

(defun combobulate-look-up-node-type (key &optional node-type-list)
  "Look up KEY in NODE-TYPE-LIST.

Where NODE-TYPE-LIST is a list of string keys or cons cells of
the form `(KEY . QUERY)'.

This function is designed for
`combobulate-navigation-default-nodes' and the like.

Return the first match."
  (setq node-type-list (or node-type-list combobulate-navigation-default-nodes))
  (or
   (seq-find (lambda (elem) (and (consp elem) (equal (car elem) key))) node-type-list)
   (seq-find (lambda (elem) (and (stringp elem) (equal elem key))) node-type-list)))


(defun combobulate-pretty-print-node-type (node)
  (or (and node (concat (mapconcat 'capitalize (split-string
                                                (combobulate-node-type node)
                                                "[_-]")
                                   " ")))
      ""))

(defun combobulate-pretty-print-node-name (node default-name)
  "Pretty prints the name of NODE"
  (if node
      (concat (mapconcat 'capitalize (split-string
                                      (combobulate-node-type node)
                                      "[_-]")
                         " "))
    default-name))

(defun combobulate-pretty-print-node (node &optional highlighted)
  "Pretty prints NODE and optionally HIGHLIGHTED"
  (combobulate--pretty-print-node node highlighted))

(defun combobulate--pretty-print-node (node &optional highlighted)
  "Internal method that pretty prints NODE and returns a string of text.

If HIGHLIGHTED then the node is highlighted with
`combobulate-tree-highlighted-node-face'. "
  (if (combobulate-node-p node)
      (let ((s (funcall combobulate-pretty-print-node-name-function node
                        (combobulate-pretty-print-node-name node ""))))
        (concat (format "%s%s"
                        (propertize s 'face
                                    (cond
                                     (highlighted 'combobulate-tree-highlighted-node-face)
                                     (t 'combobulate-tree-normal-node-face)))
                        (if combobulate-debug (concat " " (combobulate-node-range node)) ""))))
    ;; fallback in case we've got a proxy node
    (if (combobulate-proxy-node-p node)
        (combobulate-proxy-node-pp node)
      nil)))


;;; Node comparison functions

(defun combobulate--on-node-p (node)
  "Return t if the current node at point is equal to NODE"
  (or (= (combobulate-node-start node) (point))))

(defun combobulate-point-in-node-range-p (node)
  "Return t if point is contained between NODE's start and end positions"
  (and (>= (point) (combobulate-node-start node))
       (< (point) (or (combobulate-node-end node) (point-max)))))

(defun combobulate-point-in-node-overlaps-p (node)
  "Return t if point overlaps NODE's start position"
  (>= (point) (combobulate-node-start node)))

(defun combobulate-node-in-region-p (node-a)
  "Return t if NODE-A is wholly contained in a region.

Uses `point' and `mark' to infer the boundaries."
  (and
   (use-region-p)
   (>= (combobulate-node-start node-a)
       (funcall #'min (point) (mark)))
   (<= (combobulate-node-end node-a)
       (funcall #'max (point) (mark)))))

(defun combobulate-before-point-blank-p (pt)
  "Return t if there is nothing but blank text before PT."
  (save-excursion
    (goto-char pt)
    (string-blank-p
     (buffer-substring-no-properties
      (save-excursion
        (beginning-of-line)
        (point))
      (point)))))

(defun combobulate-after-point-blank-p (pt)
  "Return t if there is nothing but blank text or a newline after PT."
  (save-excursion
    (goto-char pt)
    (looking-at "[ ]*\n")))

(defun combobulate-node-contains-node-p (node-a node-b)
  "Return t if NODE-A is wholly contained inside NODE-B"
  (and node-a node-b
       (>= (combobulate-node-start node-a)
           (combobulate-node-start node-b))
       (<= (combobulate-node-end node-a)
           (combobulate-node-end node-b))))

(defun combobulate-node-before-node-p (node-a node-b)
  "Return t if NODE-A is positioned before NODE-B"
  (and node-a node-b
       (< (combobulate-node-start node-a)
          (combobulate-node-start node-b))))

(defun combobulate-node-after-node-p (node-a node-b)
  "Return t if NODE-A is positioned after NODE-B"
  (and node-a node-b
       (> (combobulate-node-start node-a)
          (combobulate-node-start node-b))))

(defun combobulate-node-larger-than-node-p (node-a node-b)
  "Return t if NODE-A is larger than NODE-B"
  (and node-a node-b
       (> (- (combobulate-node-end node-a) (combobulate-node-start node-a))
          (- (combobulate-node-end node-b) (combobulate-node-start node-b)))))

(defun combobulate-node-smaller-than-node-p (node-a node-b)
  "Return t if NODE-A is larger than NODE-B"
  (and node-a node-b
       (< (- (combobulate-node-end node-a) (combobulate-node-start node-a))
          (- (combobulate-node-end node-b) (combobulate-node-start node-b)))))

(defun combobulate-node-ends-before-node-p (node-a node-b)
  "Return t if NODE-A ends before NODE-B "
  (and node-a node-b
       (< (combobulate-node-end node-a)
          (combobulate-node-end node-b))))


(defun combobulate--query-from-node (query node &optional beg end node-only)
  "Executes QUERY against NODE and returns the results"
  (when combobulate-debug
    (treesit-query-validate (treesit-node-language node) query))
  (treesit-query-capture node query beg end node-only))

(defun combobulate-query-node-text (query node node-only)
  "Executes QUERY against NODE and retrieve their node text

If NODE-ONLY is non-nil then only the node texts are returned"
  (mapcar (lambda (node) (if node-only (combobulate-node-text node)
                      (combobulate-node-text (cdr node))))
          (combobulate-query-search node query t t)))

(defun combobulate--get-nearest-navigable-node ()
  "Returns the nearest navigable node to point"
  (combobulate-node-at-point combobulate-navigation-default-nodes))

(defun combobulate-get-parents (node)
  "Get all parent nodes of NODE"
  (reverse
   (let ((parents '()))
     (while (setq node (combobulate-node-parent node))
       (push node parents))
     parents)))

(defun combobulate-get-specific-parent-type (node specific-types &optional skip-self-similar)
  "Return the first parent with one of SPECIFIC-TYPES of NODE if such a node exist."
  (let* ((parents (combobulate-get-parents node))
         (self-similar-node nil)
         (match (seq-find (lambda (match-node)
                            (and (member (combobulate-node-type match-node) specific-types)
                                 ;; NOTE there's a special case here. If we
                                 ;; are at the beginning of a node, like a
                                 ;; JSX element, then that is also the
                                 ;; self-same type of node we're often
                                 ;; looking for as a parent. Only we're
                                 ;; going to match against ourselves and so
                                 ;; we're not getting the 'actual' parent
                                 ;; of `node'. This only happens when the
                                 ;; noce you're looking at happens to be
                                 ;; self-similarly the same as the parent.
                                 (if (and skip-self-similar (combobulate-point-at-beginning-of-node-p match-node))
                                     ;; This is the equivalent of skipping a parent, so we should not increment.
                                     (progn (setq self-similar-node match-node) nil)
                                   t)))
                          parents)))
    (cons self-similar-node match)))




(defun combobulate-find-similar-ancestors (node parents)
  "Find PARENTS of NODE that share common production rules."
  (let ((node-types (combobulate-production-rules-get-inverted (combobulate-node-type node))))
    (seq-filter
     (lambda (parent-node)
       (seq-intersection
        (combobulate-production-rules-get-inverted (combobulate-node-type parent-node))
        node-types))
     parents)))

(defun combobulate-node-visible-window-p (node &optional exclude-start exclude-end)
  "Return t if NODE is visible in the current window"
  (and node
       ;; This appears to be faster than `pos-visible-in-window-p'. More research needed.
       (or exclude-start
           (>= (combobulate-node-start node)
               (save-excursion (goto-char (point-min))
                               (forward-line (1- (line-number-at-pos (window-start))))
                               (point))))
       (or exclude-end (<= (combobulate-node-start node)
                           (save-excursion (goto-char (point-min))
                                           (forward-line (1- (line-number-at-pos (window-end))))
                                           (point))))
       ;; Too slow?
       ;; (pos-visible-in-window-p (combobulate-node-start node) (selected-window))
       ))

(defun combobulate-node-on-or-after-node-p (node-a node-b)
  "Return t if NODE-A is positioned on or after NODE-B"
  (and node-a node-b
       (>= (combobulate-node-start node-a)
           (combobulate-node-start node-b))))

(defun combobulate-point-at-node-p (node &optional end)
  "Return t if point is at the beginning (or maybe END) of NODE"
  (and (= (if end (combobulate-node-end node)
            (combobulate-node-start node))
          (point))))

(defun combobulate-node-before-point-p (node)
  "Return t if NODE's start position is < point"
  (< (combobulate-node-start node) (point)))

(defun combobulate-node-on-or-before-point-p (node)
  "Return t if NODE's start position is <= point"
  (<= (combobulate-node-start node) (point)))

(defun combobulate-node-after-point-p (node)
  "Return t if NODE's start position is > point"
  (> (combobulate-node-start node) (point)))

(defun combobulate-node-on-or-after-point-p (node)
  "Return t if NODE's start position is >= point"
  (>= (combobulate-node-start node) (point)))

(defun combobulate-point-near-node (node)
  "Return t if point is inside or at NODE."
  (or (combobulate-point-at-node-p node)
      (combobulate-point-in-node-range-p node)))

(defun combobulate-move-to-node (node &optional end)
  "Moves the point to NODE and if END is set to the end of the node."
  (unless node
    (error "Cannot move to node as it does not exist."))
  (goto-char (if end (combobulate-node-end node)
               (combobulate-node-start node)))
  node)


(defun combobulate-visual-move-to-node (node &optional end auto)
  "Move point to node, maybe the END, and then visually indicate it.

If AUTO is non-nil, then move to the end if point is at NODE's
start."
  (when node
    (combobulate-move-to-node
     node (or end (and auto (= (combobulate-node-start node) (point)))))
    (combobulate--flash-node node)
    node))

(defun combobulate-make-proxy-point-node ()
  "Create a proxy node at `point'."
  (make-combobulate-proxy-node
   :start (point)
   :end (point)
   :text ""
   :type "point"
   :named t
   :node nil
   :pp ""))

(defun combobulate--make-navigation-query ()
  "Generates a query that matches all default node types"
  `([,@(mapcar (lambda (node) (list (make-symbol node))) combobulate-navigation-default-nodes)] @node))

(defun combobulate--query-tree (query filter-fn)
  "Given QUERY build a query and filter elements with FILTER-FN"
  (when (combobulate-node-p (combobulate-buffer-root-node))
    (seq-filter filter-fn (mapcar 'cdr (combobulate--query-from-node query (combobulate-buffer-root-node))))))


(defun combobulate-get-parent-nodes (point-node possible-parents)
  "Find POSSIBLE-PARENTS on or near POINT-NODE.

POSSIBLE-PARENTS must be a list of strings or forms. If a form,
it must be a valid combobulate query that contains one or more
`@parent' labels.

Returns a list of parents ordered closest to farthest."
  (let* ((actual-parents (combobulate-get-parents point-node))
         (matched-parents))
    (reverse (seq-uniq
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
                                  (push match matched-parents))))))))))))

(defun combobulate-node-at-point (&optional node-types)
  "Return the smallest syntax node at point whose type is one of NODE-TYPES "
  (let* ((p (point))
         (node (treesit-node-on p p)))
    (if node-types
        (let ((this node))
          (catch 'done
            (while this
              (let ((smallest-node (combobulate-node-descendant-for-range
                                    this
                                    (combobulate-node-start this)
                                    (combobulate-node-start this))))
                (cond
                 ((member (combobulate-node-type this) node-types) (throw 'done this))
                 ((member (combobulate-node-type smallest-node) node-types) (throw 'done smallest-node))
                 (t (setq this (combobulate-node-parent this))))))))
      node)))

(defun combobulate--get-all-navigable-nodes-at-point ()
  "Returns all navigable nodes that start at `point'.

The returned list is ordered smallest-to-largest by the node's
extent."
  (seq-filter #'combobulate-navigable-node-p (combobulate-all-nodes-at-point)))

(defun combobulate-all-nodes-at-point (&optional backward)
  "Returns all nodes that start at `point'.

The returned list is ordered smallest-to-largest by the node's
extent."
  (let ((nodes)
        (sub-node (combobulate-node-descendant-for-range
                   (combobulate-root-node) (if backward (1- (point)) (point)) (point))))
    (while (and sub-node (= (if backward (combobulate-node-end sub-node) (combobulate-node-start sub-node)) (point)))
      (push sub-node nodes)
      (setq sub-node (combobulate-node-parent sub-node)))
    (reverse nodes)))

(defun combobulate-node-unique (nodes)
  "Removes duplicate NODES and keeps only uniques"
  (seq-uniq nodes (lambda (a b) (and (combobulate-node-eq a b)))))

(defun combobulate-node-range-extent (nodes)
  "Returns the extent -- the smallest node position and the largest -- in NODES"
  (let ((smallest most-positive-fixnum)
        (largest most-negative-fixnum))
    (mapc (lambda (c)
            (pcase-let ((`(,start . ,end) (combobulate-node-range c)))
              (when (< start smallest)
                (setq smallest start))
              (when (> end largest)
                (setq largest end))))
          nodes)
    (list smallest largest)))

(defun combobulate-filter-nodes-by-type (nodes unwanted-node-types)
  "Filter NODES of UNWANTED-NODE-TYPES."
  (while (and unwanted-node-types nodes)
    (let ((unwanted-node-type (pop unwanted-node-types)))
      (setq nodes (seq-filter (lambda (node) (not (equal (combobulate-node-type node) unwanted-node-type))) nodes))))
  nodes)

(cl-defun combobulate-filter-nodes (nodes &key keep-types remove-types (testfn nil))
  (when (and keep-types remove-types)
    (error "Cannot specify both `:keep-types' and `:remove-types'."))
  (if (and (not keep-types) (not remove-types))
      nodes
    (seq-filter (lambda (elem)
                  (let ((node-type (combobulate-node-type (if testfn
                                                              (funcall testfn elem)
                                                            elem))))
                    (if keep-types
                        (and keep-types (member node-type keep-types))
                      (if (and remove-types (member node-type remove-types))
                          nil
                        t))))
                nodes)))

(defun combobulate-node-looking-at (node-types)
  "Returns the node point is looking at if it is one of NODE-TYPES."
  (when-let (node (combobulate-node-at-point node-types))
    (when (combobulate-point-in-node-range-p node)
      node)))

(defun combobulate-node-point (node &optional end)
  "Returns the `point' of NODE at its beginning or END

If NODE is nil, then nil is returned."
  (when node
    (if end (combobulate-node-end node)
      (combobulate-node-start node))))


(defvar combobulate-skip-prefix-regexp " \t\n"
  "Skip prefix regexp used to skip past whitespace characters.")

(defvar combobulate-skip-prefix-regexp-no-newline " \t"
  "Skip prefix regexp used to skip past whitespace characters.")

(cl-defmacro with-navigation-nodes ((&key (nodes nil) skip-prefix backward (skip-newline t) (procedures nil)) &rest body)
  "Invoke BODY with a list of specific navigational nodes, and maybe advance point.

If `:nodes' is non-nil, it must be a list of legitimate tree
sitter node types to `let'-bind to
`combobulate-navigation-default-nodes'. If nil, or not specified,
use the default nodes.

If `:skip-prefix' is non-nil, then skip forward (unless
`:backward' is set) in the direction, skipping past
`combobulate-skip-prefix-regexp' characters.

If an error is raised during BODY, then reset the point to its
original position."
  (declare (indent 1) (debug (sexp body)))
  (let ((--old-pos (gensym))
        (--err (gensym)))
    `(let ((combobulate-navigation-default-nodes
            (or ,nodes
                (when (and ,procedures (not ,nodes))
                  (combobulate-procedure-get-activation-nodes ,procedures))
                combobulate-navigation-default-nodes))
           (combobulate-manipulation-default-procedures ,procedures))
       (if combobulate-debug (message "with-navigation-nodes: %s" (prin1 ,nodes)))
       ;; keep the old position around: if we skip chars around but
       ;; `body' fails with an error we want to snap back.
       (let ((,--old-pos (point))
             (,--err t))
         (when ,skip-prefix
           (if ,backward
               (skip-chars-backward
                (if ,skip-newline combobulate-skip-prefix-regexp
                  combobulate-skip-prefix-regexp-no-newline))
             (skip-chars-forward
              (if ,skip-newline combobulate-skip-prefix-regexp
                combobulate-skip-prefix-regexp-no-newline))))
         ;; preserves call stack in case of an error
         (unwind-protect
             (prog1 (progn ,@body)
               (setq ,--err nil))
           (when ,--err (goto-char ,--old-pos))
           nil)))))

(defmacro with-argument-repetition (arg &rest body)
  "Repeats BODY an ARG number of times

This is designed to handle the prefix argument and negative
modifier you can pass to many interactive movement commands."
  (declare (indent 1) (debug (atom body)))
  (let ((--arg (gensym))
        (--inc (gensym))
        (--result (gensym)))
    `(let* ((,--arg (or ,arg 1))
            (,--inc (if (> ,--arg 0) 1 -1))
            (,--result))
       (while (/= ,--arg 0)
         (setq ,--result
               (prog1 (progn ,@body)
                 (setq ,--arg (- ,--arg ,--inc)))))
       ,--result)))


(defun combobulate-navigable-node-p (node)
  "Returns non-nil if NODE is a navigable node"
  (and node (member (combobulate-node-type node) combobulate-navigation-default-nodes)))

(defun combobulate-point-at-beginning-of-node-p (node)
  "Returns non-nil if the beginning position of NODE is equal to `point'"
  (= (combobulate-node-point node) (point)))

(defun combobulate-node-blank-p (node)
  "Returns t if NODE consists of blank characters.

The function `string-blank-p' is used to determine this."
  (string-blank-p (combobulate-node-text node)))

(defun combobulate-point-at-end-of-node-p (node &optional error-margin)
  "Returns non-nil if the end position of NODE is equal to `point'

If ERROR-MARGIN is given an integer an allowance of up to
ERRROR-MARGIN in the end point position is used to determine if
`point' is considered at the end of a node."
  (or (= (combobulate-node-point node t) (point))
      (<= (abs (- (point) (combobulate-node-point node t)))
          (or error-margin 0))))

(defun combobulate--goto-node (node &optional end)
  "Moce point to the beginning position of NODE"
  (and node (goto-char (if end (combobulate-node-end node) (combobulate-node-start node)))))

(defun combobulate-nav-get-parent (node)
  "Finds a navigable parent of NODE."
  (and node
       (catch 'done
         (while (setq node (combobulate-node-parent node))
           (if (and (combobulate-navigable-node-p node)
                    (not (combobulate-point-at-beginning-of-node-p node)))
               (throw 'done node))))))

(defun combobulate-nav-get-parents (node &optional skip-current)
  "Finds all navigable parents of NODE.

SKIP-CURRENT removes all nodes where the point at the beginning
of the node."
  (seq-filter (lambda (node)
                (and (combobulate-navigable-node-p node)
                     (if skip-current
                         (not (combobulate-point-at-beginning-of-node-p node))
                       t)))
              (combobulate-get-parents node)))

(defun combobulate-nav-siblings-after-node (parent node)
  "Find the sibling after NODE. Both must share PARENT."
  (combobulate-filter-child parent
                            (lambda (match-node)
                              (combobulate-node-after-node-p match-node node))))

(defun combobulate-nav-siblings-before-node (parent node)
  "Find the sibling before NODE. Both must share PARENT."
  (reverse (combobulate-filter-child parent
                                     (lambda (match-node)
                                       (combobulate-node-after-node-p node match-node)))))


(defun combobulate-nav-get-smallest-node-at-point (&optional end)
  "Returns the smallest navigable node at point, possibly from the END"
  (seq-filter (lambda (node) (and (combobulate-navigable-node-p node)
                             (funcall (if end #'combobulate-point-at-end-of-node-p
                                        #'combobulate-point-at-beginning-of-node-p)
                                      node)))
              (combobulate-all-nodes-at-point)))

(defun combobulate-nav-forward (&optional skip-prefix)
  "Moves forward one navigable node"
  (with-navigation-nodes (:nodes combobulate-navigation-default-nodes :skip-prefix skip-prefix)
    (when-let ((node (combobulate-node-looking-at combobulate-navigation-default-nodes)))
      (when (combobulate-point-at-beginning-of-node-p node)
        node))))

(defun combobulate-nav-backward (&optional skip-prefix)
  "Moves forward one navigable node"
  (with-navigation-nodes (:nodes combobulate-navigation-default-nodes :skip-prefix skip-prefix)
    (let ((node (combobulate-node-looking-at combobulate-navigation-default-nodes)))
      (when (combobulate-point-at-end-of-node-p node 0)
        node))))

(defun combobulate-nav-logical-next ()
  (when-let* ((tree (flatten-tree
                     (combobulate-build-sparse-tree
                      'forward (remove (combobulate-node-type (combobulate-root-node)) combobulate-navigation-default-nodes)
                      #'combobulate-node-on-or-after-point-p)))
              (first-node (pop tree)))
    (when tree
      (if (combobulate-point-at-beginning-of-node-p first-node)
          (if (< (combobulate-node-end first-node) (combobulate-node-start (car tree)))
              first-node
            (seq-find #'combobulate-node-after-point-p tree))
        first-node))))

(defun combobulate-nav-logical-previous ()
  "Navigate to the logical previous node"
  (when-let* ((tree (reverse (cons
                              ;; add in `combobulate-root-node' so we
                              ;; always have a have a base node to
                              ;; return to.
                              (combobulate-root-node)
                              (flatten-tree (combobulate-build-sparse-tree
                                             'backward combobulate-navigation-default-nodes
                                             (lambda (n) (or (<= (combobulate-node-end n)
                                                            (point))
                                                        (combobulate-point-in-node-range-p n))))))))
              (first-node (pop tree)))
    (seq-find #'combobulate-node-before-point-p tree)))

(defun combobulate-nodes-share-parent-p (node-a node-b)
  "Return t if NODE-A and NODE-B have a common navigable ancesor"
  (let ((parent-a (combobulate-nav-get-parent node-a))
        (parent-b (combobulate-nav-get-parent node-b)))
    (and parent-a parent-b (combobulate-node-eq parent-a parent-b))))


(defun combobulate--get-directed-siblings (sibling direction)
  "Find siblings of SIBLING in DIRECTION"
  (let ((siblings))
    (while (setq sibling (if (eq direction 'forward)
                             (combobulate-node-next-sibling sibling)
                           (combobulate-node-prev-sibling sibling)))
      (push sibling siblings))
    (if (eq direction 'up)
        siblings
      (reverse siblings))))

(defun combobulate-find-matches-by-node-type (node-or-type node-type-list &optional keep-labels)
  (when-let (q (combobulate-look-up-node-type
                (if (combobulate-node-p node-or-type)
                    (combobulate-node-type node-or-type)
                  node-or-type)
                node-type-list))
    (cond
     ((consp q) (combobulate-query-search (car q) (cdr q) t (not keep-labels)))
     ((stringp q) node-or-type)
     (t nil))))

(defun combobulate--get-siblings (node)
  "Return all siblings of NODE."
  (mapcar #'cdr (cdr (combobulate-procedure-start-aggressive
                      (combobulate-node-start node)))))

(defun combobulate--get-sibling (node direction)
  "Returns the sibling node of NODE in the specified DIRECTION.

The sibling node is determined by the value of DIRECTION, which
can be either `backward' or `forward'. If DIRECTION is
`backward', the previous sibling of NODE is returned.

If DIRECTION is `forward', the next sibling of NODE is
returned.

If DIRECTION is `self', then NODE is resolved to the closes
self-like sibling node.

The function will aggressively try to search through the parents
of the current node if the direction if `forward'. This is done
to try and prevent point from getting stuck at the end of a node
that technically has another immediate parent."
  (let* ((siblings (combobulate--get-siblings node)))
    (cond
     ((eq direction 'forward)
      (car (seq-filter #'combobulate-node-after-point-p siblings)))
     ((eq direction 'backward)
      (car (last (seq-filter #'combobulate-node-before-point-p siblings))))
     ((eq direction 'self)
      (or (car (seq-filter #'combobulate-point-at-beginning-of-node-p siblings))
          (combobulate-point-at-beginning-of-node-p node))))))

(defun combobulate-nav-get-next-sibling (node)
  "Get the next sibling of NODE"
  (combobulate--get-sibling node 'forward))

(defun combobulate-nav-get-prev-sibling (node)
  "Get the previous sibling of NODE"
  (combobulate--get-sibling node 'backward))

(defun combobulate-nav-get-self-sibling (node)
  "Get the current (self) sibling of NODE."
  (combobulate--get-sibling node 'self))

(defun combobulate-nav-get-child (node)
  "Finds the first navigable child of NODE"
  (car-safe (seq-filter #'combobulate-node-p
                        (flatten-tree
                         (combobulate-build-sparse-tree
                          'forward combobulate-navigation-default-nodes
                          nil node)))))


(defun combobulate-forward-sexp-function-1 (backward)
  (car (seq-filter
        (lambda (node) (and (combobulate-navigable-node-p node)
                       (funcall (if backward
                                    #'combobulate-point-at-end-of-node-p
                                  #'combobulate-point-at-beginning-of-node-p)
                                node)))
        (combobulate-all-nodes-at-point backward))))

(defun combobulate-forward-sexp-function (arg)
  "Combobulate-aware function capable of navigating by sexp.

This function must be installed in `forward-sexp-function' to
work properly."
  (with-navigation-nodes (:nodes combobulate-navigation-sexp-nodes
                                 :skip-prefix t :backward (< arg 0))
    (let ((node)
          (inc (if (> arg 0) 1 -1))
          (backward (< arg 0)))
      (while (/= arg 0)
        (unless (setq node (combobulate-forward-sexp-function-1 backward))
          ;; no node found? try harder..
          (save-excursion
            (if backward
                (progn (skip-chars-forward combobulate-skip-prefix-regexp)
                       (skip-syntax-forward ". "))
              (skip-syntax-forward ". ")
              (skip-chars-forward combobulate-skip-prefix-regexp)))
          (setq node (combobulate-forward-sexp-function-1 backward)))
        (goto-char (if node
                       (if backward
                           (combobulate-node-start node)
                         (combobulate-node-end node))
                     (or (scan-sexps (point) inc) (buffer-end inc))))
        (when backward (save-excursion (backward-prefix-chars) (point)))
        (setq arg (- arg inc))))))

(defun combobulate-walk-tree (tree node-fn &optional result-fn)
  "Walk TREE applying NODE-FN to each node.

RESULT-FN is called post-order with the left and right-hand side
of the result from the recursive call. By default it calls `cons'
on the result.

NODE-FN can throw `stop' to stop walking; or `skip' to skip
 that part of the branch."
  (catch 'stop
    (combobulate-walk-tree-1 tree node-fn
                             nil
                             (or result-fn #'cons)
                             0)))

(defun combobulate-walk-tree-1 (tree node-fn leaf-fn result-fn depth)
  (pcase tree
    ;; ((guard (and (consp tree)
    ;;              (atom (car tree))
    ;;              (null (cdr tree))))
    ;;  (funcall leaf-fn tree depth))
    ((pred combobulate-node-p)
     (funcall node-fn tree depth))
    (`(,left . ,right)
     (catch 'skip
       (funcall result-fn
                (combobulate-walk-tree-1 left node-fn leaf-fn result-fn (1+ depth))
                (combobulate-walk-tree-1 right node-fn leaf-fn result-fn depth))))))


(defun combobulate-build-sparse-tree (direction match-nodes &optional match-fn start-node limit)
  "Build a sparse tree of MATCH-NODES in DIRECTION.

Optionally use MATCH-FN instead of the builtin
search (`combboulate--node-after-point-p' or
`combobulate-node-before-point-p' depending on direction).

If START-NODE is set, use it in lieu of `combobulate-root-node'."
  (with-navigation-nodes (:nodes match-nodes :backward (eq direction 'backward))
    (combobulate-induce-sparse-tree
     (or start-node (combobulate-root-node))
     (lambda (node)
       (and (combobulate-navigable-node-p node)
            (if match-fn
                (funcall match-fn node)
              (cond
               ((eq direction 'forward)
                (combobulate-node-after-point-p node))
               ((eq direction 'backward)
                (combobulate-node-before-point-p node))
               (t (error "Unknown direction `%s'" direction))))))
     nil
     limit)))

(defun combobulate-get-nodes-at-depth (tree depth)
  "Walk TREE and find all nodes at DEPTH."
  (if (zerop depth)
      (if (consp tree)
          (list (car tree))
        nil)
    (if (consp tree)
        (apply 'append (mapcar (lambda (node) (combobulate-get-nodes-at-depth node (1- depth)))
                               (cdr tree)))
      nil)))


(defun combobulate-get-immediate-siblings-of-node (node)
  "Find siblings of NODE."
  (when-let* ((children (combobulate-find-siblings node combobulate-navigation-default-nodes))
              ;; find `node' in `children'. It may be a smaller node of one
              ;; of `children' so we want to find the actual node in
              ;; `children'.
              (ctx-node (seq-find (lambda (match-node)
                                    (combobulate-node-contains-node-p node match-node))
                                  children))
              (ctx-node-pos (seq-position children ctx-node #'combobulate-node-eq)))
    (list
     ;; previous sibling
     (nth (1- ctx-node-pos) children)
     ;; `node' but (possibly the parent of it) as found in
     ;; `children'. This is important because we may be given `node'
     ;; which is "if" and thus part of "if_statement", the actual node
     ;; we want.
     ctx-node
     ;; next sibling
     (nth (1+ ctx-node-pos) children))))

(defun combobulate-find-node-in-subtree (tree node &optional starting-offset)
  "Find the subtree containing NODE in TREE.

TREE is a tree data structure in the form of a nested conses.
NODE is an element that is expected to be found in TREE.

STARTING-OFFSET is the depth at which the search for NODE should
start.  If not provided, the search starts at the root
level (depth 0).

The function returns the subtree containing NODE, or nil if NODE
is not found.

This function uses `combobulate-get-nodes-at-depth' to retrieve
the nodes at each depth and `combobulate-node-eq' to compare NODE
with the nodes at each depth.  The search terminates as soon as
NODE is found or all depths have been searched."
  (let ((subtree)
        (offset (or starting-offset 0)))
    (catch 'done
      (while (setq subtree (combobulate-get-nodes-at-depth tree offset))
        (when (seq-position subtree node #'combobulate-node-eq)
          (throw 'done subtree))
        (cl-incf offset)))))


(defun combobulate-find-siblings (node parent-node-types)
  "Given NODE find its siblings from a parent of PARENT-NODE-TYPES."
  (when-let ((nodes (seq-filter (lambda (match-node)
                                  (and (member (combobulate-node-type match-node) parent-node-types)
                                       ;; (combobulate-node-on-or-before-point-p match-node)
                                       (or (combobulate-node-before-point-p match-node)
                                           ;; dumb hack. if a node
                                           ;; begins at `point-min'
                                           ;; then it might fail the
                                           ;; previous check. However,
                                           ;; if you expand the check
                                           ;; to include nodes that
                                           ;; are before `point-min'
                                           ;; (not feasible) it may
                                           ;; not find the actual root
                                           ;; node.
                                           (and (= (combobulate-node-start match-node) (point-min))
                                                (not (combobulate-node-parent match-node))))
                                       ;; (> (combobulate-node-end match-node) (combobulate-node-end node))
                                       ))
                                (combobulate-get-parents node))))
    (combobulate-get-expanded-children (car nodes))))

(defun combobulate-get-siblings-of-node (node &optional skip-self-similar)
  (let* ((p (combobulate-get-specific-parent-type node combobulate-navigation-default-nodes
                                                  skip-self-similar))
         (parent-node (cdr p))
         (production-rules
          (list (combobulate-node-type parent-node))))
    (when parent-node
      (with-navigation-nodes (:nodes production-rules)
        (combobulate-get-expanded-children parent-node combobulate-navigation-rules-overrides)))))

(defun combobulate-production-rules-get (node-type &optional fields)
  "Get production rules for NODE-TYPE and maybe from its FIELDS.

Rules are sourced from `combobulate-navigation-rules'."
  (unless combobulate-navigation-rules
    (error "There are no production rules in `combobulate-navigation-rules'.

If you are adding a new language, ensure the grammar files are:

1. Properly built using `build/build-relationships.py', and that you have sourced the `grammar.json' and `node-types.json' files.
2. That the rebuilt `combobulate-rules.el' file is evaluated in your Emacs session.
3. That the name of the grammar matches that in `combobulate-rules.el'."))
  (setq fields (or fields '(:all t)))
  (let* ((rules (copy-sequence (cadr (assoc node-type combobulate-navigation-rules))))
         (all-keys (map-keys rules)))
    (if rules
        (apply
         #'append
         (mapcar (lambda (prop)
                   (if (plist-member rules prop)
                       (plist-get rules prop)
                     (error "Rule `%s' has no field named `%s'. Known fields: `%s'"
                            node-type prop all-keys)))
                 (cond
                  ((null fields) '(:*unnamed*))
                  ((plist-get fields :all) all-keys)
                  (t fields))))
      (error "Cannot find any rules named `%s'" node-type))))

(defun combobulate-production-rules-get-inverted (rule-name)
  "Find the inverted production rule named RULE-NAME."
  (or
   (cadr (assoc rule-name combobulate-navigation-rules-overrides-inverted))
   (cadr (assoc rule-name combobulate-navigation-rules-inverted))))

(defun combobulate-production-rules-expand (existing-rules rule fields)
  "Expand RULE with FIELDS in EXISTING-RULES and remove it."
  (setq existing-rules (delete rule existing-rules))
  (append existing-rules (combobulate-production-rules-get rule fields)))

(defun combobulate-production-rules-expand-all (rules &optional replace)
  "Expand production RULES.

If REPLACE is non-nil, then any rule in RULES is removed if it is
expanded."
  (let ((expanded-rules nil))
    (pcase-dolist (`(,rule . ,fields) rules)
      (setq expanded-rules (append expanded-rules (combobulate-production-rules-get rule fields))))
    (if replace
        (combobulate-filter-nodes-by-type expanded-rules (mapcar 'car rules))
      expanded-rules)))

(defun combobulate-production-rules-set (rule)
  "Set (and override, if it exists) RULE."
  (setq combobulate-navigation-rules-overrides
        (assoc-delete-all (car rule) combobulate-navigation-rules-overrides))
  (push (copy-tree rule) combobulate-navigation-rules-overrides))

(cl-defun combobulate-get-children (node &key (anonymous nil) excluded-fields
                                         included-fields remove-types keep-types (all t)
                                         (all-nodes nil))
  "Return the children of NODE, filtered by the specified fields.

If `:anonymous' is non-nil, return anonymous children as well.

If `:excluded-fields' is specified, return all children except
those with field names in the given list. If `:all' is non-nil,
`:excluded-fields' is allowed; otherwise, an error is signaled.

If `:included-fields' is specified, return only the children with
field names in the given list.

If neither `:excluded-fields' nor `:included-fields' is
specified, return all children if `:all' is non-nil, otherwise
signal an error.

`:excluded-fields' and `:included-fields' cannot be used
together, and cannot contain duplicate field names. If either
condition is violated, an error is signaled.

`:remove-types' is an optional list of node types to filter in
the final selection process before the children are returned.

`:keep-types' is an optional list of node types to keep.

`:all-nodes', if non-nil, matches all nodes and then applies
either `:keep-types' *or* `:remove-types'. Both are not supported
at the same time."
  ;; Set theory house keeping.
  (cond
   ((not (or excluded-fields included-fields keep-types all all-nodes))
    (error "Must have `excluded-fields' and/or `:included-fields', or `:all'"))
   ((and excluded-fields (not all))
    (error "Cannot use `:excluded-fields' unless `:all' is non-nil."))
   ((seq-intersection excluded-fields included-fields)
    (error "`:excluded-fields' and `:included-fields' share one or more field values: %s"
           (seq-intersection excluded-fields included-fields))))
  ;; We need all the rules, and the fields they belong to, so we can
  ;; act on them in turn. This is stored in
  ;; `combobulate-navigation-rules'.
  (let* ((rules (cadr (assoc (combobulate-node-type node) combobulate-navigation-rules)))
         ;; Collect the field values -- rules -- we care about. Either
         ;; what is in `:included-fields', or all of them, if that is
         ;; nil
         (allowed-types (apply #'append (mapcar (lambda (k) (plist-get rules k))
                                                (or included-fields (and all (map-keys rules)))))))
    ;; If we ask for `:all' *and* we have `:excluded-fields', then
    ;; that is equivalent to explicitly asking for the difference
    ;; between the allowed types of fields, and the fields we've
    ;; excluded. So do that. This simplifies the code paths as we can
    ;; complement the search space and avoid lots of tedious
    ;; repetition.
    (if (and all excluded-fields)
        (combobulate-get-children
         node
         :anonymous anonymous
         :included-fields (seq-difference allowed-types excluded-fields)
         :keep-types keep-types
         :remove-types remove-types
         :all nil)
      (if all-nodes
          (combobulate-filter-nodes
           (combobulate-node-children node anonymous)
           :keep-types keep-types
           :remove-types remove-types)
        (seq-filter
         (lambda (child)
           ;; If the node type is either in `:allowed-types' or
           ;; `:keep-types', we keep it *unless* it is also in
           ;; `:remove-types', which supercedes a keep request.
           (let ((node-type (combobulate-node-type child)))
             (and (or (member node-type allowed-types)
                      (member node-type keep-types))
                  (not (member node-type remove-types)))))
         (combobulate-node-children node anonymous))))))

(defun combobulate-get-expanded-children (node &optional rules)
  "Return a list of expanded children for the given NODE using RULES.

Expanded children are obtained by recursively applying the
override rules to the children of NODE. The resulting list of
nodes is sorted using the `combobulate-node-before-node-p'
function."
  (let* ((node-type (combobulate-node-type node))
         (node-rules (assoc node-type (or rules combobulate-navigation-rules-overrides)))
         (expanded-children)
         (collected))
    (pcase-let ((`(,_ . ,spec) node-rules))
      (map-let (:included-fields
                :anonymous :excluded-fields
                :expand-rules :keep-types
                :expand-nodes :remove-types)
          spec
        (setq expanded-children
              (combobulate-get-children
               node
               :anonymous anonymous
               :included-fields included-fields
               :excluded-fields excluded-fields
               :keep-types (append keep-types (combobulate-production-rules-expand-all expand-rules))
               :remove-types remove-types
               :all t))
        (setq collected expanded-children)
        (when expand-nodes
          ;; clean this up. time complexity is too high and it can be
          ;; simplified.
          (while expanded-children
            (dolist (expand-node expand-nodes)
              (let ((child (pop expanded-children)))
                (setq expanded-children
                      (nconc (and (equal (car expand-node) (combobulate-node-type child))
                                  (combobulate-get-expanded-children child (list expand-node)))
                             expanded-children))
                (setq collected (combobulate-filter-nodes-by-type
                                 (append collected expanded-children)
                                 (list (car expand-node))))))))))
    (seq-uniq (seq-sort #'combobulate-node-before-node-p collected) #'combobulate-node-eq)))

(defun combobulate-nav-to-defun (direction &optional node)
  "Navigate to a defun in DIRECTION, possibly from NODE.

DIRECTION must be `forward' or `backward'."
  (when-let* ((current-node (or node
                                (combobulate--get-nearest-navigable-node)
                                (combobulate-node-at-point)))
              (min-depth most-positive-fixnum)
              (tree (save-excursion
                      (combobulate-move-to-node current-node (eq direction 'backward))
                      (combobulate-build-sparse-tree direction combobulate-navigation-default-nodes
                                                     (if (eq direction 'backward)
                                                         #'combobulate-node-before-point-p
                                                       #'combobulate-node-on-or-after-point-p)))))
    (let ((valid-nodes)
          (current-node-depth
           ;; determine the smallest depth in the tree and also the
           ;; depth of `current-node'.
           (combobulate-walk-tree tree
                                  (lambda (leaf depth)
                                    (setq min-depth (min depth min-depth))
                                    (when (combobulate-node-eq current-node leaf)
                                      ;; once we've found
                                      ;; `current-node' in the tree,
                                      ;; we unwind the walk; no need
                                      ;; to continue.
                                      (throw 'stop depth))
                                    (cons depth leaf)))))
      ;; if we get anything but a number back, default to node depth
      ;; 0
      (when (consp current-node-depth)
        (setq current-node-depth 0))
      ;; now walk the tree again. This time it's to find matches
      ;; that satisfy `combobulate-beginning-of-defun-behavior'
      (combobulate-walk-tree
       tree
       (lambda (leaf depth)
         (when (and (if (eq 'forward direction)
                        (or (combobulate-point-near-node leaf)
                            (combobulate-node-on-or-after-point-p leaf))
                      (combobulate-node-before-point-p leaf))
                    (or
                     ;; we can always match against nodes at the
                     ;; root depth. we need this in case current
                     ;; node depth is root-level.
                     (= depth min-depth)
                     (when (eq 'backward direction)
                       (cond
                        ;;; methods of movement of use to combobulate only (probably.)
                        ((eq 'sibling-only combobulate-beginning-of-defun-behavior)
                         (= depth current-node-depth))
                        ;;; methods of movement of interest to users.
                        ;; `self-and-sibling-first' matches the defun
                        ;; we're in and any other defun at the same or
                        ;; lower depth than we're currently at
                        ((eq 'self-and-sibling-first combobulate-beginning-of-defun-behavior)
                         (<= depth current-node-depth))
                        ;; `parent' matches only nodes at a depth less
                        ;; than our current node depth, including self
                        ((eq 'parent combobulate-beginning-of-defun-behavior)
                         (or (< depth current-node-depth)
                             (combobulate-node-eq leaf current-node)))
                        ;; `root' means we only match the minimum depth.
                        ((eq 'root combobulate-beginning-of-defun-behavior)
                         (= depth min-depth))
                        ;; `linear' matches any defun at any depth
                        ((eq 'linear combobulate-beginning-of-defun-behavior) t)))))
           (push leaf valid-nodes))
         (cons depth leaf)))
      (if (eq 'forward direction)
          (car (last valid-nodes))
        (car valid-nodes)))))

(defun combobulate-nav-beginning-of-defun ()
  "Navigate backward to the beginning of defun."
  (combobulate-nav-to-defun 'backward))

(defun combobulate-nav-end-of-defun ()
  "Navigate forward to the end of the defun."
  (combobulate-nav-to-defun 'forward))

(defun combobulate-navigate-down-list-maybe (&optional arg)
  "Navigate down into a list or the nearest navigable node ARG times.

This command mimics the existing \\[down-list] command
but with added support for navigable nodes."
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:nodes combobulate-navigation-parent-child-nodes)
      (when-let ((navigable-node (combobulate--get-nearest-navigable-node))
                 (nearest-node (combobulate-node-at-point))
                 (targets (seq-filter
                           ;; Filter elements that are either nil or where the location
                           ;; it wants to jump to is _behind_ `point'. Then, find the
                           ;; minimum of the remaining elements and go to that.
                           (lambda (elem) (and elem (> elem (point))))
                           (list (save-excursion (ignore-errors (down-list 1 nil) (point)))
                                 (if (combobulate-point-at-beginning-of-node-p navigable-node)
                                     (combobulate-node-point navigable-node t))
                                 (combobulate-node-point (combobulate-nav-get-child navigable-node))
                                 (combobulate-node-point (combobulate-nav-get-child nearest-node))
                                 (ignore-errors (save-excursion
                                                  (backward-up-list 1 nil)
                                                  (forward-sexp)
                                                  (point)))))))
        (when-let (target (apply #'min targets))
          (goto-char target)
          (combobulate--flash-node (combobulate--get-nearest-navigable-node)))))))

(defun combobulate-navigate-down-list-backward-maybe (&optional arg)
  "Navigate down into a list or the nearest navigable node ARG times, backwards.

This command mimics the existing \\[down-list] command
but with added support for navigable nodes."
  (interactive "^p")
  (with-argument-repetition arg
    (with-navigation-nodes (:nodes combobulate-navigation-parent-child-nodes)
      (when-let ((navigable-node (combobulate--get-nearest-navigable-node))
                 (nearest-node (combobulate-node-at-point))
                 (targets (seq-filter
                           ;; Filter elements that are either nil or where the location
                           ;; it wants to jump to is _before_ `point'. Then, find the
                           ;; maximum of the remaining elements and go to that.
                           (lambda (elem) (and elem (< elem (point))))
                           (list (save-excursion (ignore-errors (down-list -1 nil) (point)))
                                 (if (combobulate-point-at-beginning-of-node-p navigable-node)
                                     (combobulate-node-point navigable-node t))
                                 (combobulate-node-point (combobulate-nav-get-child navigable-node))
                                 (combobulate-node-point (combobulate-nav-get-child nearest-node))
                                 (ignore-errors (save-excursion
                                                  (backward-up-list 1 nil)
                                                  (point)))))))
        (when-let (target (apply #'max targets))
          (goto-char target)
          (combobulate--flash-node (combobulate--get-nearest-navigable-node)))))))

(defun combobulate-navigate-up-list-maybe (&optional arg)
  "Maybe navigate up out of a list or to the nearest navigable node

This command mimics the existing \\[backward-up-list] command
but with added support for navigable nodes."
  (interactive "^p")
  ;; NOTE: Roll up into `combobulate-navigate-down-list-maybe'
  (with-argument-repetition arg
    (with-navigation-nodes (:nodes combobulate-navigation-parent-child-nodes)
      (let* ((navigable-node (combobulate--get-nearest-navigable-node))
             (nearest-node (combobulate-node-at-point))
             (navigable-node-parent (and navigable-node (combobulate-nav-get-parent navigable-node)))
             (nearest-node-parent (and nearest-node (combobulate-nav-get-parent nearest-node)))
             (targets (seq-filter
                       (lambda (elem) (and elem (< elem (point))))
                       (list (save-excursion (ignore-errors (backward-up-list 1 t t) (point)))
                             (combobulate-node-point navigable-node-parent)
                             (combobulate-node-point nearest-node-parent)))))
        (when-let (target (and targets (apply #'max targets)))
          (goto-char target)
          (combobulate--flash-node (combobulate--get-nearest-navigable-node)))))))

(defun combobulate-navigate-up-list-backward-maybe (&optional arg)
  "Maybe navigate up out of a list or to the nearest navigable node, backwards.

This command mimics the existing \\[up-list] command
but with added support for navigable nodes."
  (interactive "^p")
  ;; NOTE: Roll up into `combobulate-navigate-down-list-maybe'
  (with-argument-repetition arg
    (with-navigation-nodes (:nodes combobulate-navigation-parent-child-nodes)
      (let* ((navigable-node (combobulate--get-nearest-navigable-node))
             (nearest-node (combobulate-node-at-point))
             (navigable-node-parent (and navigable-node (combobulate-nav-get-parent navigable-node)))
             (nearest-node-parent (and nearest-node (combobulate-nav-get-parent nearest-node)))
             (targets (seq-filter
                       (lambda (elem) (and elem (> elem (point))))
                       (list (save-excursion (ignore-errors (up-list 1 t t) (point)))
                             (combobulate-node-point navigable-node-parent)
                             (combobulate-node-point nearest-node-parent)))))
        (when-let (target (and targets (apply #'min targets)))
          (goto-char target)
          (combobulate--flash-node (combobulate--get-nearest-navigable-node)))))))


(defun combobulate--navigate-up ()
  (with-navigation-nodes (:nodes combobulate-navigation-parent-child-nodes)
    (combobulate-nav-get-parent
     (combobulate-node-at-point))))

(defun combobulate-navigate-up (&optional arg)
  "Move up to the nearest navigable node ARG times"
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-visual-move-to-node (combobulate--navigate-up))))

(defun combobulate--navigate-down ()
  (with-navigation-nodes (:nodes combobulate-navigation-parent-child-nodes)
    (combobulate-nav-get-child
     (combobulate--get-nearest-navigable-node))))

(defun combobulate-navigate-down (&optional arg)
  "Move down into the nearest navigable node ARG times"
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-visual-move-to-node (combobulate--navigate-down))))

(defun combobulate--navigate-next ()
  (with-navigation-nodes (:skip-prefix t :procedures combobulate-navigation-sibling-procedures)
    (combobulate-nav-get-next-sibling
     (combobulate--get-nearest-navigable-node))))

(defun combobulate-navigate-next (&optional arg)
  "Move to the next navigable sibling ARG times"
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-visual-move-to-node (combobulate--navigate-next)
                                     combobulate-navigate-next-move-to-end)))

(defun combobulate--navigate-self-end ()
  (with-navigation-nodes (:skip-prefix t :procedures combobulate-navigation-sibling-procedures)
    (combobulate-nav-get-self-sibling
     (combobulate--get-nearest-navigable-node))))

(defun combobulate-navigate-self-end (&optional arg)
  "Move to the end of the current navigable node ARG times"
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-visual-move-to-node (combobulate--navigate-self-end)
                                     combobulate-navigate-next-move-to-end)))

(defun combobulate--navigate-previous ()
  (with-navigation-nodes (:procedures combobulate-navigation-sibling-procedures)
    (combobulate-nav-get-prev-sibling
     (combobulate--get-nearest-navigable-node))))

(defun combobulate-navigate-previous (&optional arg)
  "Move to the previous navigable sibling ARG times"
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-visual-move-to-node (combobulate--navigate-previous))))

(defun combobulate--navigate-forward ()
  (with-navigation-nodes (:nodes combobulate-navigation-default-nodes)
    (combobulate-nav-forward t)))

(defun combobulate-navigate-forward (&optional arg)
  "If at the beginning of a navigable node, move forward ARG times"
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-visual-move-to-node (combobulate--navigate-forward) t)))

(defun combobulate--navigate-backward ()
  (with-navigation-nodes (:nodes combobulate-navigation-default-nodes)
    (combobulate-nav-backward t)))

(defun combobulate-navigate-backward (&optional arg)
  "If at the end of a navigable node, move backward ARG times"
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-visual-move-to-node (combobulate--navigate-backward))))

(defun combobulate--navigate-logical-next ()
  (with-navigation-nodes (:nodes combobulate-navigation-logical-nodes :skip-prefix nil)
    (combobulate-nav-logical-next)))

(defun combobulate-navigate-logical-next (&optional arg)
  "Move to the next logical and navigable node ARG times"
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-visual-move-to-node (combobulate--navigate-logical-next) nil t)))

(defun combobulate--navigate-logical-previous ()
  (with-navigation-nodes (:nodes combobulate-navigation-logical-nodes)
    (combobulate-nav-logical-previous)))

(defun combobulate-navigate-logical-previous (&optional arg)
  "Move to the previous logical and navigable node ARG times"
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-visual-move-to-node (combobulate--navigate-logical-previous))))

(defun combobulate--navigate-end-of-defun ()
  (with-navigation-nodes (:nodes combobulate-navigation-defun-nodes)
    (combobulate-nav-end-of-defun)))

(defun combobulate-navigate-end-of-defun (&optional arg)
  "Navigate to the end of defun ARG times"
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-visual-move-to-node (combobulate--navigate-end-of-defun) t)))

(defun combobulate--navigate-beginning-of-defun ()
  (with-navigation-nodes (:nodes combobulate-navigation-defun-nodes)
    (combobulate-nav-beginning-of-defun)))

(defun combobulate-navigate-beginning-of-defun (&optional arg)
  "Navigate to the beginning of defun ARG times"
  (interactive "^p")
  (with-argument-repetition arg
    (combobulate-visual-move-to-node (combobulate--navigate-beginning-of-defun))))


(defun combobulate-query-build-nested-query (nodes query &optional never-merge)
  "Build a nested search QUERY of NODES.

The output is of the form:

  (A (B (C (D query))))

Where NODES is a list of (A B C D) and query is a valid
`combobulate-query-search' query.

Note that QUERY is inserted as a search query to the last node in
NODES unless NEVER-MERGE is non-nil, in which case it becomes
another subelement:

  (A (B (C (D (query)))))"
  (let ((acc query) (n) (first (not never-merge)))
    (while nodes
      (setq n (pcase (pop nodes)
                ((and (pred stringp) n)
                 (make-symbol n))
                ((and (pred symbolp) n)
                 n)
                ((and (pred combobulate-node-p) n)
                 (combobulate-query--node-type n))
                (_ (error "Unknown node type"))))
      (setq acc (cons n (if first acc (cons acc nil))))
      (setq first nil))
    acc))


(defvar combobulate-query-search-debug t)
(defvar combobulate-query--labelled-nodes nil)
(defvar combobulate-query--nested-labels nil)

(defun combobulate-query-search (query-node query &optional labelled-nodes-only no-labels nested-labels)
  "Execute QUERY starting at QUERY-NODE.

QUERY must be a well-formed query. The query language is very
similar, but not identical, to that of tree-sitter's own query
language.

Supported constructs:

  `(start-node (node))' matches start-node against query-node and
  matches if `start-node' has a child `node'.

  `(_)' can be used in lieu of an explicit node name to match any
  named node.

  `_' can be used in lieu of an explicit node name to match any
  named or anonymous node.

  `(start-node field: (match-node))' asserts that `start-node'
  must have a field named `field' followed by a `match-node'.

  `@label', `!label' and `*label' are used to name node
  sequences. `@label' returns the node; `!label' returns the
  node text; and `*label' the node type.

If LABELLED-NODES-ONLY is non-nil, then only matches with labels
are returned. Additionally, if NO-LABELS is set, the labels are
removed."
  (when combobulate-debug
    (message "Query: `%s' against `%s'" query query-node))
  (catch 'error
    (let* ((combobulate-query--labelled-nodes nil)
           (combobulate-query--nested-labels nested-labels)
           (result (combobulate-query-search-1 query-node query)))
      (if labelled-nodes-only
          (if no-labels
              (mapcar 'cdr (reverse combobulate-query--labelled-nodes))
            (reverse combobulate-query--labelled-nodes))
        result))))

(defun combobulate-query--term-type (term)
  "Return a symbol indicating the type of TERM."
  ;; (when (and (not (atom term)) (> (length term) 1))
  ;;   (princ "Term `%s' must be a length of one." term))
  (let ((type (pcase term
                ((or '* '? '+ '!)
                 'quantifier)
                ((and (pred (symbolp)) v (guard (string-prefix-p "@" (symbol-name v))))
                 'node-label)
                ((and (pred (symbolp)) v (guard (string-prefix-p "*" (symbol-name v))))
                 'type-label)
                ((and (pred (symbolp)) v (guard (string-prefix-p "!" (symbol-name v))))
                 'text-label)
                ((and (pred symbolp)
                      v (guard (string-suffix-p ":" (symbol-name v))))
                 'field)
                ('_ 'wildcard)
                ('(_) 'named-wildcard)
                ((pred stringp) 'anonymous)
                ;; I mean.. c'mon. The way to do this properly is to
                ;; pass in the state machine's state and then mark
                ;; anything a sibling query if it is not in the first
                ;; position of the whole query?
                ((and (pred consp)
                      `(,first . ,_)
                      (guard (or (consp first)
                                 (stringp first)))
                      ;; (guard (not (member first '(_ (_)))))
                      )
                 'sibling-query)
                ((pred consp) 'sub-query)
                ((pred symbolp) 'node)
                ;; (nil 'invalid)
                (_ 'invalid))))
    type))


(defun combobulate-query--node-type (n)
  (and n (intern (combobulate-node-type n))))

(defun combobulate-query-search-1 (query-node query)
  (when query-node
    (let ((parent query-node)
          (children)
          (stack)
          (term) (term-type)
          (stack-delta 0)
          ;; one of `start', `field', `node', `sibling' or `label'.
          (state 'start))
      (cl-flet* ((to-field (v) (and (symbolp v) (string-remove-suffix ":" (symbol-name v))))
                 (label-p (v) (and (symbolp v) (cond ((string-prefix-p "@" (symbol-name v))
                                                      'node-label)
                                                     ((string-prefix-p "*" (symbol-name v))
                                                      'type-label)
                                                     ((string-prefix-p "!" (symbol-name v))
                                                      'text-label))))
                 (make-label (label v)
                   (cons label (progn
                                 (setq v (if (consp v) (car v) v))
                                 (cond
                                  ((eq (label-p label) 'node-label) v)
                                  ;; NOTE: this can fail if `v' is a cons
                                  ;; with more than one element. How
                                  ;; should that be handled? Is it even
                                  ;; possible?
                                  ((eq (label-p label) 'text-label) (combobulate-node-text v))
                                  ((eq (label-p label) 'type-label) (combobulate-node-type v))
                                  (t (error "Unknown label type `%s'" (label-p label)))))))
                 (push-stack (item) (push item stack) item)
                 (pop-stack () (pop stack))
                 (state-p (expected-states)
                   (member state (if (listp expected-states) expected-states (list expected-states))))
                 (assert-state (expected-states)
                   (unless (or (not expected-states) (state-p expected-states))
                     (error "State error. Expected current state to be `%s', but it is `%s'."
                            expected-states state)))
                 (set-expected-state (new-state &optional expected-states)
                   (assert-state expected-states)

                   (setq state new-state)))

        (setq children (combobulate-node-children query-node t))

        (while query
          (setq term (pop query))
          (setq term-type (combobulate-query--term-type term))
          (pcase term
            ;; Handle the very first term in the query. This state is
            ;; always treated specially, as it's important that
            ;; `query-node' matches against the first `term'.
            ;;
            ;; This is required or node `foo' would match query `(bar)'
            ;; which is wrong.
            ((and (guard (state-p '(start))))
             (if (eq term-type 'sibling-query)
                 (progn
                   (set-expected-state 'sibling)

                   (push term query))
               (if (pcase term-type
                     ('anonymous (equal (combobulate-node-text query-node) term))
                     ('node (equal term (combobulate-query--node-type query-node)))
                     ('sub-query (combobulate-query-search-1 query-node term))
                     ((or 'wildcard 'named-wildcard) t)
                     (_ (error "Unknown start term %s" term)))
                   ;; ensure the new state is `node' as we properly matched the node
                   (progn
                     (set-expected-state 'node)
                     ;; return the query-node to the stack so it can be
                     ;; processed properly as a match.
                     (setq stack-delta 1)
                     (if (eq term-type 'node)
                         (push-stack query-node)
                       (push-stack query-node)))
                 (set-expected-state 'no-match))))
            ;; handle `field:' terms
            ((and (guard (eq term-type 'field)) (guard (state-p '(node))) field)
             (set-expected-state 'field '(node))
             (let ((rule (cadr (assoc (combobulate-node-type query-node) combobulate-navigation-rules))))
               (unless (map-contains-key rule (intern (concat ":" (to-field field))))
                 (error "Production rule for node `%s' does not support a field named `%s'. Known: `%s'"
                        query-node field (map-keys rule))))
             (push-stack (to-field field)))
            ;; Handle the labels. There are three types:
            ;;
            ;; - `@label', which maps directly the node;
            ;;
            ;; - `!label', which maps to the text of the node;
            ;;
            ;; - `*label', which maps to the type of the node.
            ;;
            ((and (guard (member term-type '(node-label text-label type-label)))
                  (guard (state-p '(node))) label)
             (when stack-delta
               (let* ((elems))
                 (dotimes (_ stack-delta)
                   (push (pop-stack) elems))
                 (dolist (e elems)
                   (push-stack (make-label label e))
                   (when combobulate-query--nested-labels
                     (push (make-label label e) combobulate-query--labelled-nodes)))
                 (unless combobulate-query--nested-labels
                   (combobulate-walk-tree elems (lambda (leaf _)
                                                  (push (make-label label leaf)
                                                        combobulate-query--labelled-nodes)))))
               (setq stack-delta 0))
             (set-expected-state 'node))
            ;; handle (_), _, "string" and ( ... )
            ;;
            ;; NOTE: turn this into a dedicate predicate that returns
            ;; the type of term.
            ((and (guard (member term-type
                                 '(anonymous
                                   sibling-query
                                   field node sub-query
                                   wildcard named-wildcard)))
                  (guard (state-p '(node field)))
                  `,sub-query)
             (let* ((starting-children children)
                    ;; determine if it is a wildcard node and what type
                    ;; (is-wildcard-node (member term-type '(named-wildcard
                    ;;                                       wildcard)))
                    ;; named-only searches apply to only some terms
                    ;; (named-only-search
                    ;;  (or (and is-wildcard-node (eq term-type 'named-wildcard))
                    ;;      ;; TODO: missing stringp? label check? field check?
                    ;;      (or (and (symbolp sub-query) (not is-wildcard-node))
                    ;;          ;; (eq term-type 'sibling-query)
                    ;;          (consp sub-query))))
                    )

               (pcase state
                 ('node
                  (pcase (funcall
                          #'combobulate-query--match-many-children
                          children
                          (if (eq term-type 'sibling-query)
                              sub-query
                            (list sub-query))
                          ;; peek at the next term. this would have been
                          ;; better handled with a prefix-style notation
                          ;; as that is in keeping with lisps'
                          ;; roots. however, the tree-sitter query
                          ;; language has postfix, and, well, so do we.
                          (let ((next-term-type (combobulate-query--term-type (car query))))
                            (cond
                             ((and (eq next-term-type 'quantifier)
                                   (state-p '(node)))
                              ;; we matched a quantifier; now get rid of
                              ;; it from the query list and pass it to
                              ;; the funcall.
                              (pop query))
                             ;; no explicit quantifier? use `1'.
                             (t '1)))
                          ;; This is a sloppy take on look-ahead for
                          ;; greedy matching: pass on a stop node -- if
                          ;; there is one -- so that the greedy
                          ;; quantifiers stop matching when they
                          ;; encounter it
                          (let ((look-ahead query))
                            (seq-find
                             (lambda (next-term)
                               (pcase (combobulate-query--term-type next-term)
                                 ((or 'field 'quantifier 'node-label 'text-label 'type-label)
                                  nil)
                                 (_ t)))
                             look-ahead))
                          (eq term-type 'sibling-query))
                    ;; `match' indicates that one or more matching
                    ;; results were found.
                    ;;
                    ;; The `remaining-children' are the ones that we
                    ;; process because
                    ;; `combobulate-query--match-children' met its test
                    ;; function and quantifier requirements.
                    (`(match . (,results . ,remaining-children))

                     (set-expected-state 'node)
                     (setq children remaining-children)
                     ;; keep tabs of how much we added to the stack this
                     ;; time around. (it'd be better if we could keep a
                     ;; pointer to our position in the list...)
                     (setq stack-delta (length results))
                     (when results (mapcar #'push-stack results)))
                    ;; `ignore' means that the quantifier (most likely)
                    ;; indicated that attempted a look-ahead (usually
                    ;; greedy) match, but failed to find anything.
                    ;;
                    ;; Because that is legal, we do not exit the search
                    ;; as we ordinarily would: instead we reset children
                    ;; to what they were when we began
                    (`(ignore . ,_)

                     (set-expected-state 'node)
                     (setq children starting-children))
                    ;; `no-match' indicates that a required match was
                    ;; attempted and failed.
                    ;;
                    ;; This is an exit event.
                    (`(no-match ,_)

                     ;; search failed. we looked ahead and found no
                     ;; children that matched. set the expected state
                     ;; to `no-match' and reset `children' to nil
                     (set-expected-state 'no-match)
                     (setq children nil))
                    ;; for everything else: throw an error.
                    (`(,unknown-tag ,rest)
                     (error "Unknown match tag returned: %s %s" unknown-tag rest))))
                 ;; this handles field matching
                 ('field
                  (set-expected-state 'node 'field)
                  (let ((matches (combobulate-query-search-1
                                  (combobulate-node-child-by-field parent (pop-stack))
                                  sub-query)))
                    (when matches
                      (push-stack matches))
                    (setq stack-delta (length matches))))
                 (_ (error "Unknown parse state for query matcher: `%s'" state)))))
            (_ (unless (state-p '(no-match))
                 (error "Query parse error: %s Query: %s" term query)))))
        ;; If we end our state with a `no-match' state, then we've clearly
        ;; failed to match against the `query'.
        ;;
        ;; If so, return `nil'. Otherwise, reverse `stack' and return
        ;; it: it holds the tree of matches.

        (if (state-p '(no-match)) nil
          (reverse stack))))))

(defun combobulate-query-find-test-function (term)
  (let ((term-type (combobulate-query--term-type term)))
    (cond
     ;; wildcard -- (_) and _ -- query terms are
     ;; handled differently from explicit nodes:
     ;;
     ;; (_) wildcards match any named node.
     ;;
     ;; _ wildcards match both anonymous and named
     ;; alike.
     ;;
     ;; Combined with `*' or `+' and it'll greedily
     ;; consume all possible matches, as per their
     ;; respective rules.
     ;;
     ;; Ordinarily, we'd recurse into the subquery
     ;; of a named cons cell node query to match;
     ;; this is not required here at all. Simply
     ;; return the item itself to match.
     ((eq term-type 'named-wildcard)
      (lambda (child _) (if (combobulate-node-named-p child)
                            (cons 'match (cons (list child) nil))
                          (cons 'no-match (cons nil nil)))))
     ((eq term-type 'wildcard)
      (lambda (child _) (cons 'match (cons (list child) nil))))
     ;; strings are handled by equality checking
     ;; the node text against `sub-query'.
     ((eq term-type 'anonymous)
      (lambda (child _)
        (if (equal term (combobulate-node-text child))
            (cons 'match (cons (list child) nil))
          (cons 'no-match (cons nil nil)))))
     ;; a single standalone (not in a cons cell)
     ;; symbol is usually the first symbol in a
     ;; query as it indicates the parent node
     ;; against which any children must match.
     ((eq term-type 'node)
      (lambda (child _)
        (if (eq term (combobulate-query--node-type child))
            (cons 'match (cons (list child) nil))
          (cons 'no-match (cons nil nil)))))
     ;; For everything else, apply recursion.
     (t (lambda (child term)
          (if-let (result (combobulate-query-search-1 child term))
              (cons 'match (cons (list result) nil))
            (cons 'no-match (cons nil nil))))))))

(iter-defun combobulate-query--iter-query (q)
  (let ((org-value q) (v))
    (while q
      (setq v (iter-yield (list (pop q) (car-safe q) (1+ (length q)))))
      (if v
          (setq q (cons 'reset org-value))
        (when (not q)
          (setq q org-value))))))

(iter-defun combobulate-query--iter-state-machine-1 ()
  (while t
    (let ((loop t) (start) (found-matches 0))
      (setq start (iter-yield 'start))
      (while loop
        (pcase start
          ('reset (setq loop nil))
          ('match
           (cl-incf found-matches)
           (setq loop nil)
           (iter-yield 'match-stop))
          ('no-children
           (iter-yield 'failed-match)
           (setq loop nil))
          ('no-match
           (setq start (iter-yield 'continue))
           (setq loop t))
          (_ (error "Unknown transition `%s'" start)))))))

(iter-defun combobulate-query--iter-state-machine-+ (strict)
  (while t
    (let ((loop t) (start) (found-matches 0))
      (setq start (iter-yield 'start))
      (while loop
        (pcase start
          ('reset (setq loop nil))
          ('match
           (cl-incf found-matches)
           (setq loop t)
           (setq start (iter-yield 'match-continue)))
          ('no-children
           (setq loop nil)
           (if (>= found-matches 1)
               (iter-yield 'match-stop)
             (iter-yield 'failed-match)))
          ('no-match
           (if (and (>= found-matches 1) strict)
               (progn
                 (setq loop nil)
                 (iter-yield 'match-stop))
             (setq loop t)
             (setq start (iter-yield 'continue))))
          (_ (error "Unknown transition `%s'" start)))))))

(iter-defun combobulate-query--iter-state-machine-* (strict)
  (while t
    (let ((loop t) (start) (found-matches 0))
      (setq start (iter-yield 'start))
      (while loop
        (pcase start
          ('reset (setq loop nil))
          ('match
           (cl-incf found-matches)
           (setq loop t)
           (setq start (iter-yield 'match-continue)))
          ('no-children
           (setq loop nil)
           (if (>= found-matches 1)
               (iter-yield 'match-stop)
             (iter-yield 'skip)))
          ('no-match
           (if (and (>= found-matches 1) strict)
               (progn
                 (setq loop nil)
                 (iter-yield 'match-stop))
             (setq loop t)
             (setq start (iter-yield 'continue))))
          (_ (error "Unknown transition `%s'" start)))))))

(iter-defun combobulate-query--iter-state-machine-? ()
  (while t
    (let ((loop t) (start) (found-matches 0))
      (setq start (iter-yield 'start))
      (while loop
        (pcase start
          ('reset (setq loop nil))
          ('match
           (cl-incf found-matches)
           (setq loop nil)
           (iter-yield 'match-stop))
          ('no-children
           (setq loop nil)
           (if (>= found-matches 1)
               (iter-yield 'match-stop)
             (iter-yield 'skip)))
          ('no-match
           (if (>= found-matches 1)
               (progn
                 (setq loop nil)
                 (iter-yield 'match-stop))
             (setq loop t)
             (setq start (iter-yield 'continue))))
          (_ (error "Unknown transition `%s'" start)))))))

(defun combobulate-query--iter-state-machine (quantifier strict)
  ;; split this out because `generator.el' has a time complexity
  ;; problem causing one large form to take an ungodly amount of
  ;; time to run.
  (pcase quantifier
    ('1 (combobulate-query--iter-state-machine-1))
    ('* (combobulate-query--iter-state-machine-* strict))
    ('+ (combobulate-query--iter-state-machine-+ strict))
    (?\ (combobulate-query--iter-state-machine-?))))

(cl-defun combobulate-query--match-many-children (children full-query &optional quantifier next-query sibling)
  (setq quantifier (or quantifier '1))
  (let* ((query-term-types (mapcar #'combobulate-query--term-type full-query))
         (match-anonymous (or (member 'anonymous query-term-types)
                              (member 'wildcard query-term-types)))
         (query (combobulate-query--iter-query full-query))
         (terms-left -1)
         (machine nil)
         (term-quantifier '1)
         (next-term)
         (match-state 'unknown)
         (machine-state nil)
         (accrued-matches)
         (child)
         (matches)
         (match-result)
         (term)
         (starting-children children))
    (when (and sibling (or (member '* full-query)
                           (member '+ full-query)))
      (error "Only `?' quantifiers are supported in sibling sub-queries."))
    (cl-flet* ((advance-machine (status)
                 (setq machine-state (iter-next machine status))
                 machine-state)
               (store-match (v)
                 (setq accrued-matches (nconc accrued-matches (mapcar #'append v))))
               (commit-matches ()
                 (setq matches (nconc matches accrued-matches))
                 (setq accrued-matches nil))
               (discard-matches ()
                 (setq accrued-matches nil))
               (next-term (&optional reset)
                 (when reset
                   (iter-next query t))
                 (pcase-let ((`(,new-term ,peek-term ,ct) (iter-next query)))
                   (setq term new-term)
                   (when (>= ct terms-left)
                     (commit-matches))
                   (setq terms-left ct)
                   (setq next-term peek-term)
                   (if (and peek-term (eq (combobulate-query--term-type peek-term) 'quantifier))
                       (setq term-quantifier peek-term)
                     (setq term-quantifier '1))
                   new-term))
               (reset-machine ()
                 (if machine
                     (setq machine-state (iter-next machine 'reset))
                   (setq machine (combobulate-query--iter-state-machine quantifier t))
                   (setq machine-state (iter-next machine)))
                 (unless (eq machine-state 'start)
                   (error "Machine start machine-state should be `start' but it is `%s'" machine-state))))
      (reset-machine)
      (next-term)
      (cl-block stop
        (while term
          ;; debug
          (when (eq (combobulate-query--term-type term) 'quantifier)
            (error "Next term is a quantifier `%s'" term))
          (cl-block next-child
            (while (setq child (pop children))
              (setq match-result nil)
              (when (or (and (not (combobulate-node-named-p child)) match-anonymous)
                        (combobulate-node-named-p child))
                (setq match-result (if (not (eq term-quantifier '1))
                                       (combobulate-query--match-many-children
                                        (list child) (list term)
                                        (prog1 (next-term) (next-term))
                                        next-term)
                                     (funcall (combobulate-query-find-test-function term) child term)))
                (pcase match-result
                  (`(match . (,sub-results . ,_))
                   (setq match-result sub-results))
                  (`(no-match . ,_)
                   (setq match-result nil))
                  (`(ignore . (nil . ,_))
                   (push child children)
                   (setq match-result nil)
                   (cl-return-from next-child))
                  (_ (error "unknown sub result `%s'" match-result)))
                (if match-result
                    (progn
                      (advance-machine 'match)
                      (when (and (eq machine-state 'match-continue)
                                 next-query children
                                 (combobulate-query-search-1 (car children)
                                                             (if (consp next-query)
                                                                 next-query
                                                               (cons next-query nil))))
                        (store-match match-result)
                        (commit-matches)
                        (setq match-state 'match)
                        (setq term nil)
                        (cl-return-from stop))
                      (pcase machine-state
                        ('match-continue
                         (store-match match-result)
                         (next-term))
                        ('match-stop
                         (store-match match-result)
                         (reset-machine)
                         (commit-matches)
                         (if (> terms-left 1)
                             (next-term)
                           (setq term nil))
                         (setq match-state 'match)
                         (cl-return-from next-child))
                        (_ (error "Positive match machine-state error: `%s'" machine-state))))
                  (pcase (setq machine-state (advance-machine 'no-match))
                    ('match-stop
                     (reset-machine)
                     (setq match-state 'match)
                     (push child children)
                     (discard-matches)
                     (cl-return-from stop))
                    ;; do nothing: we've been told to proceed
                    ('continue)
                    (_ (error "Negative match machine-state error: `%s'" machine-state))))))
            (pcase (setq machine-state (advance-machine 'no-children))
              ('failed-match
               (reset-machine)
               (setq match-state 'no-match)
               (cl-return-from stop))
              ('match-stop
               (reset-machine)
               (setq match-state 'match)
               (cl-return-from stop))
              ('skip
               (reset-machine)
               (setq children starting-children)
               (setq match-state 'ignore)
               (cl-return-from stop))
              (_ (error "Out of children machine-state error: `%s'" machine-state))))))
      (when accrued-matches
        (error "accrued matches error: `%s'" accrued-matches))
      (cons match-state (cons matches children)))))

(provide 'combobulate-navigation)
;;; combobulate-navigation.el ends here

