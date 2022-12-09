;;; combobulate-manipulation.el --- manipulate structured text with combobulate  -*- lexical-binding: t; -*-

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

(defun combobulate--get-cluster-nodes (node)
  "Find clusters of NODE

The NODE is checked against the car in
`combobulate-manipulation-node-cluster-queries' and the cdr is
the query to search for with NODE as the parent."
  (mapcar 'cdr (combobulate--query-from-node (list (alist-get (combobulate-node-type node) combobulate-manipulation-node-cluster-queries))
                                             node)))

(defun combobulate-edit-cluster-dwim ()
  "Attempt to edit a cluster of nodes"
  (interactive)
  (if combobulate-manipulation-node-cluster-queries
      (when-let ((node (combobulate-node-at-point (mapcar 'car combobulate-manipulation-node-cluster-queries)))
                 (cluster-nodes (combobulate--get-cluster-nodes node)))
        (combobulate--mc-edit-nodes cluster-nodes)
        (message "Editing %d nodes in %s" (length cluster-nodes) (combobulate-pretty-print-node node)))
    (error "There are no manipulation clusters set for this major mode")))

(defun combobulate--mc-edit-nodes (nodes &optional point-at-end)
  "Edit NODES with Multiple Cursors

Places point at the beginning of the first node unless
POINT-AT-END is non-nil."
  ;; (unless (featurep 'multiple-cursors)
  ;;   (error "Multiple Cursors is not installed or not loaded"))
  (unless multiple-cursors-mode
    (let ((counter 0))
      (dolist (node-point (mapcar (if point-at-end
                                      #'combobulate-node-end
                                    #'combobulate-node-start)
                                  nodes))
        (cl-incf counter)
        (if (= counter 1)
            (goto-char node-point)
          (save-excursion
            (goto-char node-point)
            (mc/create-fake-cursor-at-point)))))
    (mc/maybe-multiple-cursors-mode)))

(defun combobulate--kill-node (node)
  "Kill NODE in the current buffer."
  (and node (kill-region (combobulate-node-start node)
                         (combobulate-node-end node)))
  (combobulate-delete-whitespace))

(defun combobulate--mark-node (node)
  "Mark NODE in the current buffer."
  (when node
    (push-mark (combobulate-node-start node))
    (goto-char (combobulate-node-end node))
    (activate-mark)))

(defun combobulate--delete-node (node)
  "Deletes NODE in the current buffer."
  (and node (delete-region (combobulate-node-start node)
                           (combobulate-node-end node))))

(defun combobulate--replace-node (node text)
  "Replaces NODE with TEXT

The NODE is deleted (`delete-region') and TEXT inserted in its place."
  (when node
    (combobulate--goto-node node)
    (combobulate--delete-node node)
    (insert text)))

(defun combobulate-kill-node-dwim ()
  "Kill the most likely node on or near point.

The exact node that is killed will depend on the location of
point relative to the nodes in
`combobulate-navigation-default-nodes'."
  (interactive)
  (when-let ((node (combobulate--get-nearest-navigable-node))
             (pp-node (combobulate-pretty-print-node node)))
    (combobulate--kill-node node)
    (message "Killed %s" pp-node)))

(defun combobulate-mark-node-dwim ()
  "Mark the most likely node on or near point.

The exact node that is marked will depend on the location of
point relative to the nodes in
`combobulate-navigation-default-nodes'."
  (interactive)
  ;; if the mark's ahead of point then we're at the beginning of the
  ;; region; if so, swap places.
  (when (and mark-active (> (mark) (point)))
    (exchange-point-and-mark))
  (when-let ((parent (car (seq-drop-while (lambda (node) (and mark-active (>= (combobulate-node-start node) (mark))))
                                          (combobulate--nav-get-parents (combobulate-node-at-point)))))
             (pp-node (combobulate-pretty-print-node parent)))
    (combobulate--mark-node parent)
    (message "Marked %s" pp-node)))

(defun combobulate-delete-whitespace ()
  "Maybe deletes excess whitespace around point.

Whether this function does anything or not depends on
`combobulate-manipulation-trim-whitespace'."
  (cond ((eq combobulate-manipulation-trim-whitespace 'backward)
         (delete-horizontal-space t))
        ((eq combobulate-manipulation-trim-whitespace 'all)
         (delete-horizontal-space))))

(provide 'combobulate-manipulation)
;;; combobulate-manipulation.el ends here
