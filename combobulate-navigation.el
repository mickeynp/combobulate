;;; combobulate-navigation.el --- navigational aids for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
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

;;; TODO: Do not depend on this, make it dynamic.
(require 'avy)


(defun combobulate-navigable-node-p (node)
  "Returns non-nil if NODE is a navigable node"
  (and node (member (combobulate-node-type node) combobulate-navigation-default-nodes)))

(defun combobulate--point-at-beginning-of-node-p (node)
  "Returns non-nil if the beginning position of NODE is equal to `point'"
  (= (combobulate-node-point node) (point)))

(defun combobulate--point-at-end-of-node-p (node &optional error-margin)
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

(defun combobulate--nav-get-parent (node)
  "Finds a navigable parent of NODE."
  (interactive)
  (and node
       (catch 'done
         (while (setq node (combobulate-node-parent node))
           (if (combobulate-navigable-node-p node)
               (throw 'done node))))))

(defun combobulate--nav-get-parents (node &optional skip-current)
  "Finds all navigable parents of NODE.

SKIP-CURRENT removes all nodes where the point at the beginning
of the node."
  (seq-filter (lambda (node) (and (combobulate-navigable-node-p node)
                             (if skip-current
                                 (not (combobulate--point-at-beginning-of-node-p node))
                               t)))
              (combobulate-get-parents node)))

(defun combobulate--nav-get-smallest-node-at-point (&optional end)
  "Returns the smallest navigable node at point, possibly from the END"
  (seq-filter (lambda (node) (and (combobulate-navigable-node-p node)
                             (funcall (if end #'combobulate--point-at-end-of-node-p
                                        #'combobulate--point-at-beginning-of-node-p)
                                      node)))
              (combobulate-all-nodes-at-point)))

(defun combobulate--nav-forward (&optional skip-prefix)
  "Moves forward one navigable node"
  (with-navigation-nodes (:nodes combobulate-navigation-default-nodes :skip-prefix skip-prefix)
    (when-let ((node (combobulate-node-looking-at combobulate-navigation-default-nodes)))
      (when (combobulate--point-at-beginning-of-node-p node)
        node))))

(defun combobulate--nav-backward (&optional skip-prefix)
  "Moves forward one navigable node"
  (with-navigation-nodes (:nodes combobulate-navigation-default-nodes :skip-prefix skip-prefix)
    (let ((node (combobulate-node-looking-at combobulate-navigation-default-nodes)))
      (forward-char 1)
      (when (combobulate--point-at-end-of-node-p node 0)
        node))))

(defun combobulate--nodes-share-parent-p (node-a node-b)
  "Returns t if NODE-A and NODE-B have a common navigable ancesor"
  (let ((parent-a (combobulate--nav-get-parent node-a))
        (parent-b (combobulate--nav-get-parent node-b)))
    (and parent-a parent-b (combobulate-node-eq parent-a parent-b))))


(defun combobulate--nav-get-sibling-nodes (node filter-fn)
  (let* ((parent-node (combobulate--nav-get-parent node))
         (nodes (seq--into-list (and parent-node
                                     (mapcar 'cdr (combobulate--query-from-node (combobulate--make-navigation-query) parent-node))))))
    (seq-filter filter-fn nodes)))

(defun combobulate--nav-get-next-sibling (node)
  (car (combobulate--nav-get-sibling-nodes node (lambda (match-node)
                                                  (and match-node node
                                                       (combobulate--node-after-node-p match-node node)
                                                       (combobulate--nodes-share-parent-p match-node node))))))


(defun combobulate--nav-get-prev-sibling (node)
  (car (reverse (combobulate--nav-get-sibling-nodes
                 node (lambda (match-node)
                        (and match-node node
                             (combobulate--node-before-node-p match-node node)
                             (combobulate--nodes-share-parent-p match-node node)))))))


(defun combobulate--nav-get-child (node)
  "Finds the first navigable child of NODE"
  (with-navigation-nodes (:nodes combobulate-navigation-sexp-nodes :skip-prefix t :backward nil)
    (let ((node (combobulate--get-nearest-navigable-node))
          (nodes (seq--into-list (mapcar 'cdr (combobulate--query-from-node (combobulate--make-navigation-query)
                                                                            node)))))
      (car (seq-filter (lambda (match-node) (combobulate--node-after-node-p match-node node)) nodes)))))

(defun combobulate-avy-jump ()
  "Use avy to jump to a navigable node"
  (interactive)
  ;; Is this right?
  (avy-process (mapcar (lambda (node) (cons (cons (combobulate-node-start node)
                                             (combobulate-node-end node))
                                       (selected-window)))
                       (combobulate--query-tree (combobulate--make-navigation-query)
                                                #'combobulate--node-visible-window-p))))

(defun combobulate-forward-sexp-function (arg)
  "Combobulate-aware function capable of navigating by sexp.

This function must be installed in `forward-sexp-function' to
work properly."
  (with-navigation-nodes (:nodes combobulate-navigation-sexp-nodes :backward (< arg 0))
    (let ((node)
          (inc (if (> arg 0) 1 -1))
          (backward (< arg 0)))
      (while (/= arg 0)
        (funcall (if backward #'skip-chars-backward #'skip-chars-forward) combobulate-skip-prefix-regexp)
        (setq node
              (car (seq-filter
                    (lambda (node) (and (combobulate-navigable-node-p node)
                                   (funcall (if backward
                                                #'combobulate--point-at-end-of-node-p
                                              #'combobulate--point-at-beginning-of-node-p)
                                            node)))
                    (combobulate-all-nodes-at-point backward))))
        (goto-char (if node
                       (if backward
                           (combobulate-node-start node)
                         (combobulate-node-end node))
                     (or (scan-sexps (point) inc) (buffer-end inc))))
        (when backward (save-excursion (backward-prefix-chars) (point)))
        (setq arg (- arg inc))))))

(defun combobulate-build-sparse-tree (direction match-nodes &optional match-fn start-node)
  (with-navigation-nodes (:nodes match-nodes :backward (eq direction 'backward))
    (treesit-induce-sparse-tree
     (or start-node (combobulate-root-node))
     (or match-fn
         (lambda (node)
           (and (combobulate-navigable-node-p node)
                (cond
                 ((eq direction 'forward)
                  (combobulate--node-after-point-p node))
                 ((eq direction 'backward)
                  (combobulate--node-before-point-p node))
                 (t (error "Unknown direction `%s'" direction)))))))))

(defun combobulate-end-of-defun (&optional arg)
  (interactive)
  (when-let (tree (combobulate-build-sparse-tree 'forward combobulate-navigation-defun-nodes))
    (combobulate-move-to-node (car (flatten-tree (cdr tree))))))

(defun combobulate-beginning-of-defun (&optional arg)
  (interactive)
  (when-let (tree (combobulate-build-sparse-tree 'backward combobulate-navigation-defun-nodes))
    (combobulate-move-to-node (car (last (flatten-tree (last tree)))))))


(provide 'combobulate-navigation)
;;; combobulate-navigation.el ends here
