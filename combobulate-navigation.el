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
  (and node (member (tsc-node-type node) combobulate-navigation-node-types)))

(defun combobulate--point-at-beginning-of-node-p (node)
  "Returns non-nil if the beginning position of NODE is equal to `point'"
  (= (combobulate-node-point node) (point)))

(defun combobulate--point-at-end-of-node-p (node &optional error-margin)
  "Returns non-nil if the end position of NODE is equal to `point'

If `error-margin' is given an integer an allowance of up to
`error-margin' in the end point position is used to determine if
`point' is considered at the end of a node."
  (or (= (combobulate-node-point node t) (point))
      (<= (abs (- (point) (combobulate-node-point node t)))
          (or error-margin 0))))

(defun combobulate--goto-node (node &optional end)
  "Moce point to the beginning position of NODE"
  (and node (goto-char (if end (tsc-node-end-position node) (tsc-node-start-position node)))))

(defun combobulate--nav-get-parent (node)
  "Finds a navigable parent of NODE."
  (interactive)
  (and node
       (catch 'done
         (while (setq node (tsc-get-parent node))
           (if (combobulate-navigable-node-p node)
               (throw 'done node))))))

(defun combobulate--nav-get-parents (node)
  "Finds all navigable parents of NODE."
  (seq-filter #'combobulate-navigable-node-p (combobulate--get-parents node)))

(defun combobulate--nav-get-smallest-node-at-point ()
  "Returns the smallest navigable node that starts at `point'."
  (seq-filter (lambda (node) (and (combobulate-navigable-node-p node)
                             (combobulate--point-at-beginning-of-node-p node)))
              (combobulate-all-nodes-at-point)))

(defun combobulate--nav-forward (&optional skip-prefix)
  "Moves forward one navigable node"
  (when skip-prefix
    (skip-chars-forward "[\s-\s.]+\n"))
  (let ((node (combobulate-node-looking-at combobulate-navigation-node-types)))
    (when (combobulate--point-at-beginning-of-node-p node)
      node)))

(defun combobulate--nav-backward (&optional skip-prefix)
  "Moves forward one navigable node"
  (when skip-prefix
    (skip-chars-backward "[\s-\s.]+\n")
    ;; Required so that we 'enter' the node immediately behind
    ;; point. Being on the right-hand side of a node with `point'
    ;; means you are technically outside it!
    (forward-char -1))
  (let ((node (combobulate-node-looking-at combobulate-navigation-node-types)))
    (forward-char 1)
    (when (combobulate--point-at-end-of-node-p node 0)
      node)))

(defun combobulate--nodes-share-parent-p (node-a node-b)
  "Returns t if NODE-A and NODE-B have a common navigable ancesor"
  (let ((parent-a (combobulate--nav-get-parent node-a))
        (parent-b (combobulate--nav-get-parent node-b)))
    (and parent-a parent-b (tsc-node-eq parent-a parent-b))))


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
  (let ((nodes (seq--into-list (mapcar 'cdr (combobulate--query-from-node (combobulate--make-navigation-query) node)))))
    (car (seq-filter (lambda (match-node) (combobulate--node-after-node-p match-node node)) nodes))))

(defun combobulate-avy-jump ()
  "Use avy to jump to a navigable node"
  (interactive)
  ;; Is this right?
  (avy-process (mapcar (lambda (node) (cons (cons (tsc-node-start-position node)
                                             (tsc-node-end-position node))
                                       (selected-window)))
                       (combobulate--query-tree (combobulate--make-navigation-query)
                                                #'combobulate--node-visible-window-p))))


(provide 'combobulate-navigation)
;;; combobulate-navigation.el ends here
