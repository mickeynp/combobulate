;;; combobulate-display.el --- display hierarchies of structured code  -*- lexical-binding: t; -*-

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
(require 'combobulate-ztree)
(require 'subr-x)

(defvar combobulate--display-tree nil
  "Internal storage for the sparse display tree")


;;; Tree drawing code

(defvar combobulate-display-glyphs '((vbar . "│")
                                     (prev-next-sibling . "├")
                                     (hbar . "─")
                                     (next-sibling . "┌")
                                     (prev-sibling . "└")
                                     (spacer . " "))
  "Glyphs to use for the tree display")

(defun combobulate--display-tree-node (ctx)
  (let-alist combobulate-display-glyphs
    (or (pcase ctx
          ;; interstitial nodes
          (`((:prev-sibling . t)
             (:next-sibling . t))
           .vbar)
          (`((:prev-sibling . nil)
             (:next-sibling . t))
           .spacer)
          (`((:prev-sibling . t)
             (:next-sibling . nil))
           .spacer)
          (`((:prev-sibling . nil)
             (:next-sibling . nil))
           .spacer)
          ;; leaf nodes
          ;; ----------
          ;; maybe parents, but no siblings
          (`((:parents . ,parents)
             (:prev-sibling . nil)
             (:next-sibling . nil))
           (if (> parents 0)
               (concat .prev-sibling .hbar)
             (concat .spacer .hbar)))
          ;; maybe parents, but one next sibling
          (`((:parents . ,parents)
             (:prev-sibling . nil)
             (:next-sibling . t))
           (if (> parents 0)
               (concat .prev-next-sibling .hbar)
             (concat .next-sibling .hbar)))
          ;; maybe parents, but one previous sibling
          (`((:parents . ,_)
             (:prev-sibling . t)
             (:next-sibling . nil))
           (concat .prev-sibling .hbar))
          ;; maybe parents, both siblings
          (`((:parents . ,_)
             (:prev-sibling . t)
             (:next-sibling . t))
           (concat .prev-next-sibling .hbar)))
        ;; fallback
        .spacer)))


(defun combobulate-display-tree-parents (loc)
  (let ((parents))
    (while (setq loc (combobulate-ztree-up loc))
      ;; parent of root node is root node.. which is a bit odd.
      (when (and (combobulate-ztree-up loc) (car-safe (combobulate-ztree-node (combobulate-ztree-up loc))))
        (when-let (node (car (combobulate-ztree-node loc)))
          (push loc parents))))
    (reverse parents)))

(defun combobulate-display--determine-tree-node-context (tree)
  (cons (cons :parents (length (combobulate-display-tree-parents tree)))
        (combobulate-display--determine-parent-tree-node-context tree)))

(defun combobulate-display--determine-parent-tree-node-context (tree)
  (list
   ;; bit of a hacky work-around as `combobulate-ztree-lefts' and `combobulate-ztree-rights'
   ;; seem to think the children are the siblings and not the other
   ;; way around?!
   (cons :prev-sibling (not (null (thread-first tree
                                                (combobulate-ztree-up)
                                                (combobulate-ztree-lefts)))))
   (cons :next-sibling (not (null (thread-first tree
                                                (combobulate-ztree-up)
                                                (combobulate-ztree-rights)))))))

(defun combobulate-display--draw-node (loc &optional highlighted)
  "Draws a navigation node NODE, and optionally HIGHLIGHTED, with tree guides"
  (let ((guides)
        (parents (combobulate-display-tree-parents loc))
        (ctx (combobulate-display--determine-tree-node-context loc)))
    (dotimes (offset (length parents))
      (push (concat (combobulate--display-tree-node
                     (combobulate-display--determine-parent-tree-node-context
                      (nth offset parents)))
                    (alist-get 'spacer combobulate-display-glyphs))
            guides))
    (let ((node-text (combobulate-pretty-print-node (combobulate-ztree-node loc))))
      (string-join
       (list
        (propertize (apply 'concat guides) 'face 'combobulate-tree-branch-face)
        (propertize (combobulate--display-tree-node ctx) 'face 'combobulate-tree-branch-face)
        (propertize "" 'face 'combobulate-tree-branch-face)
        (propertize (string-trim node-text) 'face
                    (cond
                     ((and (not (combobulate-ztree-branch-p loc))
                           (combobulate-node-in-region-p (combobulate-ztree-node loc)))
                      'highlight)
                     (highlighted 'combobulate-tree-highlighted-node-face)
                     (t 'combobulate-tree-normal-node-face))))
       ""))))

;;; Sparse tree code -- used for performance reasons instead of
;;; `combobulate-nav-xxxx'.

(defun combobulate-display-make-tree (&optional start-node depth)
  "Build a sparse tree around the parent of the navigable node at point"
  (when-let ((node (or start-node (thread-first (combobulate--get-nearest-navigable-node)
                                                (combobulate-node-parent)
                                                (combobulate-node-parent)
                                                (combobulate-node-parent)))))
    (let ((tree (combobulate-induce-sparse-tree
                 node #'combobulate-navigable-node-p nil (or depth 3))))
      ;; the tree may start with a `nil' node; if so, ditch it, as it
      ;; messes with the tree hierarchy.
      (combobulate-ztree-list-zip (if (car tree) tree (cadr tree))))))

(defun combobulate-display-draw-node-tree (node)
  "Renders a navigation tree in node-list mode around NODE"
  (with-navigation-nodes (:nodes (seq-difference
                                  combobulate-navigation-default-nodes
                                  combobulate-display-ignored-node-types))
    (when (member (combobulate-node-type node) combobulate-display-ignored-node-types)
      (setq node (combobulate--get-nearest-navigable-node)))
    (when node
      (save-excursion
        (combobulate-display-draw-tree-1 (combobulate-display-create-locus node) node)))))


(defun combobulate-display-create-locus (start-node)
  (let* ((parent (combobulate-nav-get-parent start-node))
         (grand-parent (combobulate-nav-get-parent parent))
         (ztree (combobulate-ztree-list-zip (list (or grand-parent parent))))
         (children
          (cons (if grand-parent parent nil)
                (mapcar (lambda (n)
                          (cons n (mapcar #'list
                                          (seq-remove #'combobulate-node-blank-p
                                                      (seq-filter #'combobulate-navigable-node-p
                                                                  ;; make this configurable
                                                                  (seq-take (combobulate-node-children n) 1))))))
                        (seq-uniq
                         (seq-remove #'combobulate-node-blank-p
                                     (seq-filter #'combobulate-navigable-node-p
                                                 (or (combobulate-get-immediate-siblings-of-node start-node)
                                                     (list
                                                      (combobulate-node-prev-sibling start-node)
                                                      start-node
                                                      (combobulate-node-next-sibling start-node))))))))))
    (combobulate-ztree-append-child
     ztree
     (if grand-parent children (cons children nil)))))

(defun combobulate-display-draw-tree-1 (tree &optional highlighted-node)
  (let ((drawing))
    (while (and (setq tree (combobulate-ztree-next tree))
                (not (combobulate-ztree-end-p tree)))
      (when (not (combobulate-ztree-branch-p tree))
        (push (combobulate-display--draw-node
               tree (and highlighted-node
                         (or (equal (combobulate-ztree-node tree)
                                    highlighted-node))))
              drawing)))
    (string-join (reverse drawing) "\n")))

(defun combobulate-display-draw-complete-tree (start-node target-buf &optional depth)
  (let* ((tree (combobulate-display-make-tree start-node depth)))
    (with-current-buffer (get-buffer-create target-buf)
      (erase-buffer)
      (insert (combobulate-display-draw-tree-1 tree)))))


(provide 'combobulate-display)
;;; combobulate-display.el ends here
